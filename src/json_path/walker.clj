(ns json-path.walker)

(declare walk eval-expr)

(defn eval-eq-expr [op-form context operands]
  (apply op-form (map #(eval-expr % context) operands)))

(defn eval-expr [[expr-type & operands :as expr] context]
  (let [ops {:eq =, :neq not=, :lt <, :lt-eq <=, :gt >, :gt-eq >=}]
    (cond
     (contains? ops expr-type) (eval-eq-expr (expr-type ops) context operands)
     (= expr-type :val) (first operands)
     (= expr-type :path) (walk expr context))))

(defn select-by [[opcode & operands :as obj-spec] context]
  (cond
   (sequential? (:current context)) (vec (flatten (filter #(not (empty? %))
                                                          (map #(select-by obj-spec (assoc context :current %))
                                                               (:current context)))))
   :else (cond
          (= (first operands) "*") (vec (vals (:current context)))
          :else ((keyword (first operands)) (:current context)))))

(defn obj-vals [obj]
  (cond
    (seq? obj) obj
    (map? obj) (filter map? (vals obj))
    :else []))

(defn obj-aggregator [obj]
  (let [obj-vals (obj-vals obj)
        children (flatten (map obj-aggregator obj-vals))]
    (vec (concat obj-vals children))))

(defn walk-path [[next & parts] context]
  (cond
   (nil? next) (:current context)
   (= [:root] next) (walk-path parts (assoc context :current (:root context)))
   (= [:child] next) (walk-path parts context)
   (= [:current] next) (walk-path parts context)
   (= [:all-children] next) (walk-path parts (assoc context :current (vec (concat [(:current context)]
                                                                                  (obj-aggregator (:current context))))))
   (= :key (first next)) (walk-path parts (assoc context :current (select-by next context)))))

(defn walk-selector [sel-expr context]
  (cond
   (= :index (first sel-expr)) (if (sequential? (:current context))
                                 (let [sel (nth sel-expr 1)]
                                   (if (= "*" sel)
                                     (:current context)
                                     (nth (:current context) (Integer/parseInt sel))))
                                 (throw (Exception. "object must be an array.")))
   (= :filter (first sel-expr)) (filter #(eval-expr (nth sel-expr 1) (assoc context :current %)) (:current context))))

(defn walk [[opcode operand continuation] context]
  (let [down-obj (cond
         (= opcode :path) (walk-path operand context)
         (= opcode :selector) (walk-selector operand context))]
    (if continuation
      (walk continuation (assoc context :current down-obj))
      down-obj)))


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

(defn walk-path-ng [[next & parts] context]
  (cond
    (nil? next) [(:current context) []]
    (= [:root] next) (walk-path-ng parts (assoc context :current (:root context)))
    (= [:child] next) (walk-path-ng parts context)
    (= [:current] next) (walk-path-ng parts context)
    (= [:all-children] next) (walk-path parts (assoc context :current (vec (concat [(:current context)]
                                                                                   (obj-aggregator (:current context))))))
    (= :key (first next)) (let [[value key] (select-by-ng next context)
                                [result downstream-key] (walk-path-ng parts (assoc context :current value))]
                            [result (cons key downstream-key)])))

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
