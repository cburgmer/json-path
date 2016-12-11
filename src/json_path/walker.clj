(ns json-path.walker)

(declare walk eval-expr)

(defn eval-eq-expr [op-form context operands]
  (apply op-form (map #(eval-expr % context) operands)))

(defn eval-expr [[expr-type & operands :as expr] context]
  (let [ops {:eq =, :neq not=, :lt <, :lt-eq <=, :gt >, :gt-eq >=}]
    (cond
     (contains? ops expr-type) (eval-eq-expr (expr-type ops) context operands)
     (= expr-type :val) (first operands)
     (= expr-type :path) (first (walk expr context)))))

(defn select-by [[opcode & operands :as obj-spec] context]
  (cond
   (sequential? (:current context)) (let [sub-selection (->> (:current context)
                                                             (map #(select-by obj-spec (assoc context :current %)))
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

(defn obj-vals [obj]
  (cond
    (seq? obj) (map-indexed (fn [idx child-obj] [child-obj [idx]]) obj)
    (map? obj) (->> obj
                    (filter (fn [[k v]] (map? v)))
                    (map (fn [[k v]] [v [k]])))
    :else '()))

(defn obj-aggregator [obj]
  (let [obj-vals (obj-vals obj)
        children (->> obj-vals
                      (mapcat (fn [[val key]] (->> (obj-aggregator val)
                                                   (map (fn [[child-val child-key]] [child-val (vec (concat key child-key))]))))))]
    (concat obj-vals children)))

(defn walk-path [[next & parts] context]
  (cond
   (nil? next) [(:current context) []]
   (= [:root] next) (walk-path parts (assoc context :current (:root context)))
   (= [:child] next) (walk-path parts context)
   (= [:current] next) (walk-path parts context)
   (= [:all-children] next) (let [children (obj-aggregator (:current context))
                                  all-children (cons [(:current context) []] children)
                                  sub-selection (->> all-children
                                                     (map (fn [[obj key]] (let [[child-val child-key] (walk-path parts (assoc context :current obj))]
                                                                            [child-val (vec (concat key child-key))])
                                                            ;; also no map
                                                            ))
                                                     (filter #(not (empty? (first %)))))]
                              (transpose sub-selection))
   (= :key (first next)) (let [[value key] (select-by next context)
                               [result downstream-key] (walk-path parts (assoc context :current value))]
                           [result (vec (concat key downstream-key))])))

(defn walk-selector [sel-expr context]
  (cond
   (= :index (first sel-expr)) (if (sequential? (:current context))
                                 (let [sel (nth sel-expr 1)]
                                   (if (= "*" sel)
                                     (let [current (:current context)]
                                       [current (vec (map vector (range (count current))))])
                                     (let [index (Integer/parseInt sel)]
                                       [(nth (:current context) index) [index]])))
                                 (throw (Exception. "object must be an array.")))
   (= :filter (first sel-expr)) (->> (:current context)
                                     (keep-indexed (fn [i e] (if (eval-expr (nth sel-expr 1) (assoc context :current e)) [e [i]])))
                                     (apply map vector))))


(defn walk [[opcode operand continuation] context]
  (let [[down-obj down-key] (cond
                              (= opcode :path) (walk-path operand context)
                              (= opcode :selector) (walk-selector operand context))]
    (if continuation
      (let [[obj key] (walk continuation (assoc context :current down-obj))]
        [obj (vec (concat down-key key))])
      [down-obj down-key])))
