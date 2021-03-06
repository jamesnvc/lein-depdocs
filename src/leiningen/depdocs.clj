(ns leiningen.depdocs
  (:require
    [leiningen.core.eval :as lein]))

(defn depdocs
  "Generate dependency documentation"
  [project & args]
  (lein/eval-in-project
    (-> project (update :dependencies conj ['lein-depdocs "0.1.0-SNAPSHOT"]))
    `(depdocs.core/find-dependencies '~(:source-paths project))
    '(require 'depdocs.core)))
