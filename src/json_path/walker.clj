(ns json-path.walker)

(declare walk eval-expr)

(defn map# [func obj]
  (if (seq? obj)
    (map func obj)
    (func obj)))

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
                                                             (keep-indexed (fn [i sel] (map# (fn [[obj key]] (if (not (empty? obj)) [obj (vec (cons i key))])) sel))))]
                                      (if (seq? (first sub-selection))
                                        (apply concat sub-selection)
                                        sub-selection))
   :else (cond
           (= (first operands) "*") (map (fn [[k v]] [v [k]]) (:current context))
           :else (let [key (keyword (first operands))]
                   [(key (:current context)) [key]]))))

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

(defn- with-parent-key [parent-key selection]
  (map# (fn [[value key]] [value (vec (concat parent-key key))]) selection))

(defn walk-path [[next & parts] context]
  (cond
   (nil? next) [(:current context) []]
   (= [:root] next) (walk-path parts (assoc context :current (:root context)))
   (= [:child] next) (walk-path parts context)
   (= [:current] next) (walk-path parts context)
   (= [:all-children] next) (let [children (obj-aggregator (:current context))
                                  all-children (cons [(:current context) []] children)
                                  sub-selection (->> all-children
                                                     (map (fn [[obj key]] (with-parent-key key (walk-path parts (assoc context :current obj)))))
                                                     (filter #(not (empty? (first %)))))]
                              (if (seq? (first sub-selection))
                                (apply concat sub-selection)
                                sub-selection))
   (= :key (first next)) (let [selection (select-by next context)
                               result (map# (fn [[value key]] (with-parent-key key (walk-path parts (assoc context :current value)))) selection)]
                           result)))

(defn walk-selector [sel-expr context]
  (cond
   (= :index (first sel-expr)) (if (sequential? (:current context))
                                 (let [sel (nth sel-expr 1)]
                                   (if (= "*" sel)
                                     (map-indexed (fn [idx child-obj] [child-obj [idx]]) (:current context))
                                     (let [index (Integer/parseInt sel)]
                                       [(nth (:current context) index) [index]])))
                                 (throw (Exception. "object must be an array.")))
   (= :filter (first sel-expr)) (keep-indexed (fn [i e] (if (eval-expr (nth sel-expr 1) (assoc context :current e)) [e [i]]))
                                              (:current context))))

(defn walk [[opcode operand continuation] context]
  (let [down (cond
              (= opcode :path) (walk-path operand context)
              (= opcode :selector) (walk-selector operand context))]
    (if continuation
      (if (seq? down)
        (map (fn [[val key]] (let [[child-val child-key] (walk continuation (assoc context :current val))] ;; no map?
                               [child-val (vec (concat key child-key))]))
             down)
        (let [[val key] down
              obj (walk continuation (assoc context :current val))]
          (map# (fn [[child-val child-key]] [child-val (vec (concat key child-key))]) obj)))
      down)))
