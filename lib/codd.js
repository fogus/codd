//    codd.js
//    http://www.functionaljs.org/codd
//    (c) 2013 Fogus, Ariadne Softworks
//    codd.js may be freely distributed under the MIT license

// *the secret JavaScript sauce*
(function() {
  // Imports
  // -------
  var L = this.L || require('lemonad');

  // The base Codd object
  var Codd = function(array) {
    // TODO: Implement more efficiently
    var set = {};
    var results = [];

    for (index in array) {
      set[JSON.stringify(array[index])] = array[index];
    }

    for(key in set) {
      results.push(set[key]);
    }

    return results;
  };

  // Current version
  Codd.VERSION = '0.7.1';

  L.uniq = Codd;
  L.Codd = Codd;
  Codd.toSet = Codd;
  Codd.keys  = Object.keys;

  // Relational algebra

  // *signature:* `any* -> [any*]`

  L.$ = function(){
    return L.uniq(arguments);
  };

  Codd.difference = function(minus) {
    return function(target) {
      return L.filter(function(value){ 
        return !L.existy(L.has(L.eq(value))(minus)); 
      }, target);
    };
  };

  Codd.restrict = function(xset, pred) {
    return L.reduce(function(result, val) {
      if (L.truthy(pred(val)))
        return result;
      else
        return Codd.difference([val])(result);
    }, xset, xset);
  };

  Codd.selectKeys = function(keys) {
    return function(rel) {
      var copy = {};

      for(var key in keys) {
        if (keys[key] in rel) copy[keys[key]] = rel[keys[key]];
      };

      return copy;
    };
  };

  Codd.project = function(table, ks) {
    return L.map(function(key) {
      var rel = table[key];
      return Codd.selectKeys(ks)(rel);
    }, Codd.keys(table));
  };

  Codd.omitKeys = function(minus) {
    return function(rel) {
      var copy = {};
      var value;

      for (var key in rel) {
        if (!L.existy(L.has(L.eq(key))(minus)))
          copy[key] = rel[key];
      }

      return copy;
    };  
  };

  Codd.renameKeys = function(obj, kobj) {
    var seed = Codd.omitKeys(Codd.keys(kobj))(obj);

    return L.reduce(function(o, key) {
      if (L.existy(obj[key])) o[kobj[key]] = obj[key];

      return o;
    },
    seed,
    Codd.keys(kobj));
  };

  Codd.rename = function(table, mappings) {
    return L.map(L.rcurry2(Codd.renameKeys)(mappings), table);
  };

  Codd.lookup = function(index, obj, alt) {
    var answer = alt;

    L.has(function(entry) {
      var key = L.first(entry);

      if (L.eq(key)(obj)) {
        answer = L.tail(entry);
        return true;
      }

      return false;
    })(index);

    return answer;
  };

  Codd.put = function(index, key, element) {
    var found = false;
    var result = L.map(function(entry) {
      if (L.eq(L.first(entry))(key)) {
        found = true;
        return L.cat([key, element], L.tail(entry));
      }

      return entry;
    }, index);

    return found ? result : L.cons([key, element], result);
  };

  Codd.index = function(table, ks) {
    return L.reduce(function(index, rel) {
      var ik = Codd.selectKeys(ks)(rel);
      var iv = Codd.lookup(index, ik, Codd());

      return Codd.put(index, ik, rel);
    }, [], table);
  };

  Codd.naturalJoin = function(table1, table2) {
    var xks = _.keys(_.first(table1));
    var yks = _.keys(_.first(table2));
    var ks  = _.intersection(xks, yks);

    var sz = (_.size(table1) <= _.size(table2)) ? [table1, table2] : [table2, table1];
    var r  = sz[0];
    var s  = sz[1];

    var idx = Codd.index(r, ks);

    return _.reduce(s, function(result, o){
      var found = Codd.lookup(idx, _.selectKeys(o, ks));

      if (_.existy(found))
        return _.reduce(found, function(agg, rel){
          return _.cons(_.merge(rel, o), agg);
        }, result);
      else
        return result;
    }, []);
  };

  Codd.RQL = {
    where:   L.rcurry2(Codd.restrict),
    select:  L.rcurry2(Codd.project),
    as:      L.rcurry2(Codd.rename)
  };

  // Exports
  // -------

  var root = this;

  if (typeof exports !== 'undefined') {
    if (typeof module !== 'undefined' && module.exports) {
      exports = module.exports = Codd;
    }
    exports.Codd = Codd;
  } else {
    root.Codd = Codd;
  }

  // *Now mix into Lemonad*
  L.mix(L.$.prototype, Codd);

}).call(this);

