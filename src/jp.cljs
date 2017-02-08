(ns jp
  (:require json-path))

(defn ^:export query [path object]
  (clj->js (json-path/query path (js->clj object))))

(defn ^:export at-path [path object]
  (clj->js (json-path/at-path path (js->clj object))))
