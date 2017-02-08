# json-path

An implementation of the [JsonPath][jp] spec for Clojure.

It doesn't provide a full implementation. The following syntactic
elements are supported.

* `$`: Select the root of the object.
* `.`: Descend to a single child.
* `..`: Recursive descent to select all children.
* `*` and `<name>`: Select the child objects matching a name, or all
  immediate children when using `*`. Unlike in JavaScript, the `name`
  can contain hyphens (`-`). This is convenient as Clojure data
  structures often use hyphens in keys.
* `[<number>]` and `[*]`: Select either a specific element of an
  array, or all elements of an array.
* `[?(<expr>)]`: Filter to select objects that match an
  expression. Supports all equality operators (`=`, `!=`, `<`, `<=`,
  `>`, `>=`) and can use `@` to refer to objects from the current
  object.

See [`test/json_path/test/json_path_test.clj`][eg] for more examples.

## Missing

Notable features missing from the JsonPath spec are multiple array
indices (`[,]`),array slicing (`[start:end:step]`) and arbitrary
script expressions (`[(...)]`). Also, the set of expression operators
should probably be expanded.

## Future Work

`json-path` will never support the arbitrary script expressions
feature. That's just a plain bad idea.

JsonPath is missing a parent operator. This is necessary for
performing cousin operations. I propose using `^` as the parent
operator. It would be used anywhere that the `.` child operator could
be used.

The supported expression operators should be expanded. And as
arbitrary script expressions won't be supported, functions for
operations like length should be added.

## Usage

Accepts standard Clojure data structures: maps and sequences. Assumes
that maps contains keywords for the child objects.

    (json-path/at-path "$.foo" {:foo {:bar "Hello, world!"}})
    ; => {:bar "Hello, world!"}

    (->> (json-path/query "$..bar" {:foo {:bar "Hello"}})
         (map :path))
    ; => ([:foo :bar])

## Contributors

* [Giles Alexander](https://github.com/gga)
* [Matthew Gertner](https://github.com/matthewgertner)
* [Christoph Burgmer](https://github.com/cburgmer)

## JS build

```
$ curl -L https://github.com/clojure/clojurescript/releases/download/r1.9.293/cljs.jar -O
$ java -cp cljs.jar:src clojure.main build.clj
```

Then in the browser: `jp.at_path('$..', {foo: "bar", baz: {hello: 'world'}})`

## License

Copyright (C) 2011 Giles Alexander

Distributed under the MIT License.

[jp]: http://goessner.net/articles/JsonPath/
[eg]: https://github.com/gga/json-path/blob/master/test/json_path/test/json_path_test.clj
