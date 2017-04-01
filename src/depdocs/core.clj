(ns depdocs.core
  (:require
    [clojure.java.io :as io]
    [clojure.tools.namespace.dependency :as tnd]
    [clojure.tools.namespace.dir :as tndir]
    [clojure.tools.namespace.find :as tnf]
    [clojure.tools.namespace.track :as tnt]))

(defn find-dependencies
  [source-paths]
  (prn "Source paths" source-paths)
  (let [source-path-dirs (map io/file source-paths)
        project-nses (mapcat tnf/find-namespaces-in-dir source-path-dirs)
        project-graph (tndir/scan-dirs (tnt/tracker) source-path-dirs)]
    (println "Analyzing nses" project-nses)
    (doseq [project-ns project-nses]
      (prn project-ns)
      (require project-ns)
      (let [publics (ns-publics project-ns)
            dependents (tnd/immediate-dependents (::tnt/deps project-graph) project-ns)]
        (prn "publics" publics)
        (println project-ns "is used by" dependents)
        (println "Analyzing " (count publics) "public vars")
        ))))
