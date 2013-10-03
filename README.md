Codd
====

A functional programming library built on [Lemonad](http://www.functionaljavascript.org) to provide relational algebra operations in JavaScript.

## Using

Add the following to your 'package.json' file in the `"dependencies"` section:

    "codd": "0.8.3"

## Currently available functions:

```javascript
[ 'Codd.difference',
  'Codd.index',
  'Codd.intersection',
  'Codd.keys',
  'Codd.lookup',
  'Codd.naturalJoin',
  'Codd.omitKeys',
  'Codd.project',
  'Codd.put',
  'Codd.rename',
  'Codd.renameKeys',
  'Codd.restrict',
  'Codd.selectKeys',
  'Codd.toSet',
  'Codd.union',
  'Codd.RQL.as',
  'Codd.RQL.field',
  'Codd.RQL.like',
  'Codd.RQL.select',
  'Codd.RQL.where' ]
```

Influences / References
-----------------------

* *[SQL and Relational Theory: How to Write Accurate SQL Code](http://www.amazon.com/gp/product/1449316409/?tag=fogus-20)* by CJ Data
* [Clojure and ClojureScript](http://www.clojuredocs.org)
* [Underscore.js](http://underscorejs.org/)
* [Functional JavaScript](http://osteele.com/sources/javascript/functional/) by Oliver Steele
* Functional JavaScript (the book)

## License

This software is provided as-is under the [MIT license](http://opensource.org/licenses/MIT).
