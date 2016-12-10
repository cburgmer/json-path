(ns json-path.walker)

(declare walk eval-expr)

(defn eval-eq-expr-ng [op-form context operands]
  (apply op-form (map #(eval-expr-ng % context) operands)))

(defn eval-expr-ng [[expr-type & operands :as expr] context]
  (let [ops {:eq =, :neq not=, :lt <, :lt-eq <=, :gt >, :gt-eq >=}]
    (cond
      (contains? ops expr-type) (eval-eq-expr-ng (expr-type ops) context operands)
      (= expr-type :val) (first operands)
      (= expr-type :path) (first (walk-ng expr context)))))

(defn select-by-ng [[opcode & operands :as obj-spec] context]
  (cond
    (sequential? (:current context)) (let [sub-selection (->> (:current context)
                                                              (map #(select-by-ng obj-spec (assoc context :current %)))
                                                              (keep-indexed (fn [i [obj key]] (if (not (empty? obj)) [obj (vec (cons i (flatten key)))]))))]
                                       [(vec (flatten (map first sub-selection))) (map second sub-selection)])
    :else (cond
            (= (first operands) "*") [(vec (vals (:current context))) (map vector (keys (:current context)))]
            :else (let [key (keyword (first operands))]
                    [(key (:current context)) [key]]))))

(defn- transpose [pairs]
  (if (empty? pairs)
    [[] []]
    (apply mapv vector pairs)))

(defn obj-vals-ng [obj]
  (cond
    (seq? obj) (transpose [obj (map vector (range (count obj)))])
    (map? obj) (->> obj
                    (filter (fn [[k v]] (map? v)))
                    (map (fn [[k v]] [v [k]])))
    :else '()))

(defn obj-aggregator-ng [obj]
  (let [obj-vals (obj-vals-ng obj)
        children (->> obj-vals
                      (mapcat (fn [[val key]] (->> (transpose (obj-aggregator-ng val))
                                                   (map (fn [[child-val child-key]] [child-val (vec (concat key child-key))]))))))]
    (transpose (concat obj-vals children))))

(defn walk-path-ng [[next & parts] context]
  (cond
    (nil? next) [(:current context) []]
    (= [:root] next) (walk-path-ng parts (assoc context :current (:root context)))
    (= [:child] next) (walk-path-ng parts context)
    (= [:current] next) (walk-path-ng parts context)
    (= [:all-children] next) (let [children (transpose (obj-aggregator-ng (:current context)))
                                   all-children (cons [(:current context) []] children)
                                   sub-selection (map (fn [[obj key]] (let [[child-val child-key] (walk-path-ng parts (assoc context :current obj))]
                                                                           [child-val key])  ;; child-key?
                                                           )
                                                         all-children)]
                               (transpose sub-selection))
    (= :key (first next)) (let [[value key] (select-by-ng next context)
                                [result downstream-key] (walk-path-ng parts (assoc context :current value))]
                            [result (concat key downstream-key)])))

(defn walk-selector-ng [sel-expr context]
  (cond
    (= :index (first sel-expr)) (if (sequential? (:current context))
                                  (let [sel (nth sel-expr 1)]
                                    (if (= "*" sel)
                                      (let [current (:current context)]
                                        [current (range (count current))])
                                      (let [index (Integer/parseInt sel)]
                                        [(nth (:current context) index) [index]])))
                                  (throw (Exception. "object must be an array.")))
    (= :filter (first sel-expr)) (->> (:current context)
                                      (keep-indexed (fn [i e] (if (eval-expr-ng (nth sel-expr 1) (assoc context :current e)) [e i])))
                                      (apply map vector))))

(defn walk-ng [[opcode operand continuation] context]
  (let [[down-obj down-key] (cond
                   (= opcode :path) (walk-path-ng operand context)
                   (= opcode :selector) (walk-selector-ng operand context))]
    (if continuation
      (let [[obj key] (walk-ng continuation (assoc context :current down-obj))]
        [obj (vec (concat down-key key))])
      [down-obj down-key])))
