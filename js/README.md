Codd
====

A functional programming library built on [Lemonad](http://www.functionaljs.org) to provide relational algebra operations in JavaScript.

## Use

Add the following to your 'package.json' file in the `"dependencies"` section:

    "codd": "0.8.3"

### Node (require)

Install the package and its dependency:

```bash
npm install codd lemonad
```

Then in Node:

```javascript
const L = require('lemonad');
const Codd = require('codd'); // or './lib/codd' locally
```

### Browser

```html
<script src="/path/to/lemonad.js"></script>
<script src="/path/to/codd.js"></script>
```

## Currently available functions:

```
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

Examples
--------

```javascript
// simple RQL example
const table1 = [{a: 1, b: 2}, {a: 2, b: 4}];

// project/select (returns array of objects with only the given keys)
Codd.RQL.select(['a'])(table1);
// -> [{ a: 1 }, { a: 2 }]

// where/restrict (filters rows via a predicate)
Codd.RQL.where(L.isOdd)([1,2,3,4,5]);
// -> [1, 3, 5]

// as
Codd.RQL.as({'a':'AAA'})(table1);
// -> [ { b: 2, AAA: 1 }, { b: 4, AAA: 2 } ]

// natural join example
const people = [
    {id: 1, name: 'Alice', deptId: 10},
    {id: 2, name: 'Bob',   deptId: 20},
    {id: 3, name: 'Carol', deptId: 10}
];

const depts = [{deptId: 10, dept: 'Engineering'}, {deptId: 20, dept: 'Sales'}];

Codd.naturalJoin(people, depts)
// -> [{ deptId: 10, dept: 'Engineering', id: 3, name: 'Carol' },
//     { deptId: 20, dept: 'Sales', id: 2, name: 'Bob' },
//     { deptId: 10, dept: 'Engineering', id: 3, name: 'Carol' }]


// Composed example
const TUNES_DB = [
  {artist: 'Burial', title: 'Archangel', genre: 'dubstep', year: 2007},
  {artist: 'Donovan', title: 'Sunshine Superman', genre: 'folk', year: 1966},
  {artist: 'Ikonika', title: 'Idiot', genre: 'dubstep', year: 2010},
  {artist: 'The Beatles', title: 'Hey Jude', genre: 'rock', year: 1968}
];

Codd.RQL.q(
  TUNES_DB,
  Codd.RQL.where(Codd.RQL.field('genre', Codd.RQL.like('dubstep'))),
  Codd.RQL.select(['artist', 'title']),
  Codd.RQL.as({'title': 'songTitle'})
);

// -> [{artist: 'Burial', songTitle: 'Archangel'},
//     {artist: 'Ikonika', songTitle: 'Idiot'}]
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
