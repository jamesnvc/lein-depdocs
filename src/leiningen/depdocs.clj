(ns leiningen.depdocs
  (:require
    [clojure.java.io :as io]
    [clojure.tools.namespace.dependency :as tnd]
    [clojure.tools.namespace.dir :as tndir]
    [clojure.tools.namespace.find :as tnf]
    [clojure.tools.namespace.track :as tnt]
    [leiningen.core.eval :as lein]))

(defn depdocs
  "I don't do a lot."
  [project & args]
  (let [project-nses (tnf/find-namespaces-in-dir (io/file (:root project) "src"))
        project-graph (tndir/scan-dirs (tnt/tracker) (map io/file (:source-paths project)))]
    (println "Analyzing nses" project-nses)
    (doseq [project-ns (reverse project-nses)]
      (prn project-ns)
      (let [#_publics #_(lein/eval-in-project
                      project
                      (list 'ns-publics project-ns)
                      (list 'require project-ns))
            dependents (tnd/immediate-dependents (::tnt/deps project-graph) project-ns)]
        (println project-ns "is used by" dependents)
        ;(println "Analyzing " (count publics) "public vars")
        ))))
