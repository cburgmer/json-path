(ns json-path.test.walker-test
  [:use [json-path.walker]
   [midje.sweet]])

(unfinished)

(facts
  (eval-expr-ng [:eq [:val "a"] [:val "b"]] {}) => falsey
  (eval-expr-ng [:eq [:val "a"] [:val "a"]] {}) => truthy
  (eval-expr-ng [:neq [:val "a"] [:val "b"]] {}) => truthy
  (eval-expr-ng [:lt [:val 10] [:val 11]] {}) => truthy
  (eval-expr-ng [:lt-eq [:val 10] [:val 10]] {}) => truthy
  (eval-expr-ng [:gt [:val 10] [:val 9]] {}) => truthy
  (eval-expr-ng [:gt-eq [:val 10] [:val 10]] {}) => truthy
  (eval-expr-ng [:path [[:key "foo"]]] {:current {:foo "bar"}}) => "bar"
  (eval-expr-ng [:eq [:path [[:key "foo"]]] [:val "bar"]] {:current {:foo "bar"}}) => truthy)

(facts
  (select-by-ng [:key "hello"] {:current {:hello "world"}}) => ["world" [:hello]]
  (select-by-ng [:key "hello"] {:current [{:hello "foo"} {:hello "bar"}]}) => [["foo" "bar"] [[0 :hello] [1 :hello]]]
  (select-by [:key "hello"] {:current [{:blah "foo"} {:hello "bar"}]}) => [ "bar"]
  (select-by [:key "*"] {:current {:hello "world"}}) => ["world"]
  (sort (select-by [:key "*"] {:current {:hello "world", :foo "bar"}})) => ["bar", "world"]
  (sort (select-by [:key "*"] {:current [{:hello "world"}, {:foo "bar"}]})) => ["bar", "world"])

(fact
  (walk-path-ng [[:root]] {:root ...root..., :current  ...obj...}) => [...root... []]
  (walk-path-ng [[:root] [:child] [:key "foo"]] {:root {:foo "bar"}}) => ["bar" [:foo]]
  (walk-path [[:all-children]] {:current {:foo "bar" :baz {:qux "zoo"}}}) => [{:foo "bar" :baz {:qux "zoo"}},
                                                                   {:qux "zoo"}])

(fact
  (walk-selector-ng [:index "1"] {:current ["foo", "bar", "baz"]}) => ["bar" [1]]
  (walk-selector-ng [:index "*"] {:current [:a :b]}) => [[:a :b] [0 1]]
  (walk-selector-ng [:filter [:eq [:path [[:current] [:child] [:key "bar"]]] [:val "baz"]]]
                 {:current  [{:bar "wrong"} {:bar "baz"}]}) => [[{:bar "baz"}] [1]])

(fact "selecting places constraints on the shape of the object being selected from"
  (walk-selector-ng [:index "1"] {:current {:foo "bar"}}) => (throws Exception)
  (walk-selector-ng [:index "*"] {:current {:foo "bar"}}) => (throws Exception))

(facts
  (walk-ng [:path [[:root]]] {:root ...json...}) => [...json... []]
  (walk-ng [:path [[:child]]] {:current ...json...}) => [...json... []]
  (walk-ng [:path [[:current]]] {:current ...json...}) => [...json... []]
  (walk-ng [:path [[:key "foo"]]] {:current {:foo "bar"}}) => ["bar" [:foo]]
  (walk [:path [[:all-children]]]
        {:current
         {:hello {:world "foo"},
          :baz {:world "bar",
                :quuz {:world "zux"}}}}) => [{:hello {:world "foo"},
                                              :baz {:world "bar", :quuz {:world "zux"}}},
                                             {:world "foo"},
                                             {:world "bar",
                                              :quuz {:world "zux"}},
                                             {:world "zux"}]
  (walk [:path [[:all-children]]]
        {:current
         (list {:hello {:world "foo"}}
               {:baz {:world "bar"}})}) => [[{:hello {:world "foo"}}
                                             {:baz {:world "bar"}}]
                                            {:hello {:world "foo"}}
                                            {:baz {:world "bar"}}
                                            {:world "foo"}
                                            {:world "bar"}]
  (walk [:path [[:all-children]]]
        {:current "scalar"}) => ["scalar"]
  (walk-ng [:selector [:index "1"]] {:current ["foo", "bar", "baz"]}) => ["bar" [1]]
  (walk-ng [:selector [:index "*"]] {:current [:a :b]}) => [[:a :b] [0 1]]
  (walk-ng [:selector [:filter [:eq
                             [:path [[:current]
                                     [:child]
                                     [:key "bar"]]]
                             [:val "baz"]]]]
           {:current [{:bar "wrong"} {:bar "baz"}]}) => [[{:bar "baz"}] [1]]
  (walk-ng [:path [[:root] [:child] [:key "foo"]]
            [:selector [:filter [:eq [:path [[:current]
                                             [:child]
                                             [:key "bar"]]]
                                 [:val "baz"]]]
             [:path [[:child] [:key "hello"]]]]]
           {:root {:foo [{:bar "wrong" :hello "goodbye"}
                         {:bar "baz" :hello "world"}]}}) => [["world"] [:foo 1 :hello]])

(facts "walking a nil object should be safe"
  (walk [:path [[:root]]] nil) => nil
  (walk [:path [[:root] [:child] [:key "foo"]]] {:bar "baz"}) => nil
  (walk [:path [[:root] [:child] [:key "foo"] [:child] [:key "bar"]]]
        {:foo {:baz "hello"}}) => nil)
