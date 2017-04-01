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

;; From lein-kibit
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

(defn zipper
  [data]
  (z/zipper
    coll?
    seq
    (fn [n c]
      (-> (cond
            (vector? n) (vec c)
            (map? n) (into {} c)
            true c)
        (with-meta (meta n))))
    data))

(defn analyze-source-for
  [source-seq syms]
  (loop [z (zipper source-seq)
         found {}]
    (cond
      (z/end? z) found

      (and (symbol? (z/node z)) (syms (z/node z)))
      (recur (z/next z)
             (let [f (-> z z/up z/node)]
               (update found
                      (symbol (name (z/node z)))
                      conj
                      {:form f
                       :line (-> f meta :line)
                       :col (-> f meta :column)})))

      :else (recur (z/next z) found))))

(defn qualified-symbols
  "Get a set of the symbols that the public vars from `project-ns` can be
  referred to as in `dependent`"
  [project-ns dependent]
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
    (set/union aliases full refers)))

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
                  (->> (with-open [r (io/reader (ns->file ns))]
                         (read-file (LineNumberingPushbackReader. r) ns))
                       rest ; drop the namespace declaration
                       (swap! sources assoc ns))))]
      (doall
        (for [project-ns project-nses]
          (do (binding [*out* *err*] (require project-ns))
              (let [dependents (tnd/immediate-dependents (::tnt/deps project-graph) project-ns)]
                (println project-ns)
                (println "==========")
                (prn)
                (let [pub-uses (->>
                                 (for [dependent dependents]
                                   (do
                                     (binding [*out* *err*] (require dependent))
                                     (->> (analyze-source-for
                                          (source-of dependent)
                                          (qualified-symbols project-ns dependent))
                                        (into
                                          {}
                                          (map (fn [[sym uses]]
                                                 [sym (map #(assoc % :ns dependent) uses)]))))))
                                 (apply merge-with concat))]
                  (doseq [[sym sym-used] pub-uses]
                    (println "## Uses of `" sym "`")
                    (doseq [{:keys [line col form ns]} sym-used]
                      (println "At line" line "column" col "of" ns)
                      (println)
                      (println "```")
                      (pprint/pprint form)
                      (println "```")))))))))))
