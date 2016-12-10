(ns json-path
  [:require [json-path.parser :as parser]
   [json-path.walker :as walker]])

(defn at-path [path object]
  (first (walker/walk-ng (parser/parse-path path) {:root object})))
