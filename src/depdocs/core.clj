(ns depdocs.core
  (:require
    [clojure.java.io :as io]
    [clojure.pprint :as pprint]
    [clojure.set :as set :refer [map-invert]]
    [clojure.string :as str]
    [clojure.tools.namespace.dependency :as tnd]
    [clojure.tools.namespace.dir :as tndir]
    [clojure.tools.namespace.find :as tnf]
    [clojure.tools.namespace.track :as tnt]
    [clojure.zip :as z])
  (:import
    (clojure.lang LineNumberingPushbackReader)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; From kibit - https://github.com/jonase/kibit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- careful-refer
  "Refers into the provided namespace all public vars from clojure.core
except for those that would clobber any existing interned vars in that
namespace.  This is needed to ensure that symbols read within syntax-quote
end up being fully-qualified to clojure.core as appropriate, and only
to *ns* if they're not available there.  AFAICT, this will work for all
symbols in syntax-quote except for those referring to vars that are referred
into the namespace."
  [ns]
  (binding [*ns* ns]
    (refer 'clojure.core :exclude (or (keys (ns-interns ns)) ())))
  ns)

(def eof (Object.))

; making this non-lazy to avoid "file already closed" issue
(defn read-file
  "Generate a sequence of top level forms from a LineNumberingPushbackReader"
  [^LineNumberingPushbackReader r init-ns]
  (let [do-read (fn do-read [ns]
                  (let [form (binding [*ns* ns] (read r false eof))
                        [ns? new-ns k] (when (sequential? form) form)
                        ns (if (and (symbol? new-ns)
                                    (or (= ns? 'ns) (= ns? 'in-ns)))
                             (careful-refer (create-ns new-ns))
                             ns)]
                    (when-not (= form eof)
                      (cons form (do-read ns)))))]
    (do-read (careful-refer (create-ns init-ns)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn debug
  [& args]
  (binding [*out* *err*]
    (apply prn args)))

(defn tee [x] (debug x) x)

(defn zipper
  [data]
  (z/zipper
    coll?
    seq
    (fn [n c]
      (-> (cond
            (vector? n) (vec c)
            (map? n) (into {} c)
            :else c)
        (with-meta (meta n))))
    data))

(def sym-name (comp symbol name))

(defn nearest-numbered
  [z]
  (loop [z z]
    (cond
      (z/end? z) nil

      (:line (meta (z/node z)))
      (-> z z/node meta (select-keys [:line :column]) (assoc :form (z/node z)))

      :else (recur (z/up z)))))

(defn find-uses
  [source-seq syms]
  (loop [z (zipper source-seq)
         found {}]
    (cond
      (z/end? z) found

      (and (symbol? (z/node z)) (syms (z/node z)))
      (recur (z/next z)
             (update found
                     (sym-name (z/node z))
                     conj
                     (nearest-numbered z)))

      :else (recur (z/next z) found))))

(defn qualified-symbols
  "Get a set of the symbols that the public vars from `project-ns` can be
  referred to as in `dependent`"
  [project-ns dependent]
  (if (= project-ns dependent)
    (set (keys (ns-publics project-ns)))
    (let [?alias (->> (ns-aliases dependent)
                      (filter (fn [[alias ns]] (= (ns-name ns) project-ns)))
                      ffirst)
          publics (ns-publics project-ns)
          aliases (when ?alias
                    (into
                      #{}
                      (map (fn [s] (symbol (str ?alias) (str s))))
                      (keys publics)))
          full (into
                 #{}
                 (map (fn [s] (symbol (str project-ns) (str s))))
                 (keys publics))
          refers (-> (ns-refers dependent)
                     map-invert
                     (select-keys (vals publics))
                     vals
                     set)]
      (set/union aliases full refers))))

(defn remove-definitions
  "Remove usages of a var that are just its own definition"
  [var-meta uses]
  (->> uses (remove (fn [{l :line}] (= l (:line var-meta))))))

(defn analyze
  [{:keys [uses-from in-target target-source]}]
  (->> (find-uses
         target-source
         (qualified-symbols uses-from in-target))
       (into
         {}
         (comp (map (fn [[sym uses]]
                      [sym (map #(assoc % :ns in-target) uses)]))
               (if (= uses-from in-target)
                 (let [pubs (ns-publics uses-from)]
                   (map (fn [[sym uses]]
                          [sym (remove-definitions (meta (pubs (sym-name sym))) uses)])))
                 (map identity))))))

(defn print-uses
  [project-ns pub-uses]
  (println project-ns)
  (println "==========")
  (prn)
  (let [pubs (ns-publics project-ns)]
    (doseq [[sym sym-uses] pub-uses]
      (println "## Uses of " (str "`" project-ns "/" sym "`"))
      (println)
      (println "Defined on line" (:line (meta (get pubs sym))))
      (println)
      (doseq [[used-ns sym-used] sym-uses]
        (println "### Uses in " (str "`" used-ns "`"))
        (println)
        (doseq [{:keys [line column form ns]} (sort-by :line sym-used)]
          (println "At line" line "column" column)
          (println)
          (println "```clojure")
          (pprint/pprint form)
          (println "```"))))))

(defn find-dependencies
  [source-paths]
  (let [source-path-dirs (map io/file source-paths)
        project-nses (mapcat tnf/find-namespaces-in-dir source-path-dirs)
        project-graph (tndir/scan-dirs (tnt/tracker) source-path-dirs)
        sources (atom {})]
    (binding [*out* *err*] (println "Analyzing nses" project-nses))
    (letfn [(ns->file [ns]
              (let [base (-> ns str (str/replace #"\." "/")
                             (str/replace #"-" "_"))]
                (->> (for [prefix source-path-dirs
                           suffix ["clj" "cljc"]]
                       (io/file prefix (str base "." suffix)))
                     (filter (memfn exists))
                     first)))
            (source-of [ns]
              (or (@sources ns)
                  (-> (with-open [r (io/reader (ns->file ns))]
                        (read-file (LineNumberingPushbackReader. r) ns))
                      rest ; drop the namespace declaration
                      (->> (swap! sources assoc ns))
                      (get ns))))]
      (doseq [project-ns project-nses]
        (binding [*out* *err*] (require project-ns))
        (let [dependents (tnd/immediate-dependents (::tnt/deps project-graph) project-ns)
              pub-uses (->> (for [dependent (conj dependents project-ns)]
                              (do
                                (binding [*out* *err*] (require dependent))
                                (analyze
                                  {:uses-from project-ns
                                   :in-target dependent
                                   :target-source (source-of dependent)})))
                            (apply merge-with concat)
                            (map (fn [[sym uses]]
                                   [sym (group-by :ns uses)])))]
          (print-uses project-ns pub-uses))))))
