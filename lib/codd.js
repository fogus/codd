
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
  Codd.VERSION = '0.8.1';

  L.uniq = Codd;
  L.Codd = Codd;
  Codd.toSet = Codd;
  Codd.keys  = Object.keys;

  // *signature:* `any* -> [any*]`
  L.$ = function(){
    return L.uniq(arguments);
  };

  // TODO: Reimplement more efficiently
  Codd.intersection = function(lhs) {
    return function(rhs) {
      return L.filter(function(elem) {
        return rhs.indexOf(elem) != -1;
      }, lhs);
    };
  };

  // TODO: Reimplement more efficiently
  Codd.union = function(lhs) {
    return function(rhs) {
      return L.uniq(L.cat(lhs, rhs));
    };
  };

  // TODO: Reimplement more efficiently
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
    var xks = Codd.keys(L.first(table1));
    var yks = Codd.keys(L.first(table2));
    var ks  = Codd.intersection(xks)(yks);

    var sz = (L.len(table1) <= L.len(table2)) ? [table1, table2] : [table2, table1];
    var r  = sz[0];
    var s  = sz[1];

    var idx = Codd.index(r, ks);

    return L.reduce(function(result, o){
      var found = Codd.lookup(idx, Codd.selectKeys(ks)(o));

      if (L.existy(found))
        return L.reduce(function(agg, rel){
          return L.cons(L.merge(rel, o), agg);
        }, result, found);
      else
        return result;
    }, [], s);
  };

  Codd.RQL = {
    where:   L.rcurry2(Codd.restrict),
    select:  L.rcurry2(Codd.project),
    as:      L.rcurry2(Codd.rename),
    q:       L.pipeline
  };

  Codd.tunes = [{band: "Blind Lemon Jefferson", plays: 141, genre: "Delta Blues"},
                {band: "Erroll Garner",         plays: 115, genre: "Jazz Piano"},
                {band: "Brian Eno",             plays: 94,  genre: "Electronic"},
                {band: "Charlie Parker",        plays: 75,  genre: "Bebop Jazz"},
                {band: "Alice Coltrane",        plays: 73,  genre: "Free Jazz"},
                {band: "Tim Hecker",            plays: 70,  genre: "Ambient"},
                {band: "Charles Mingus",        plays: 69,  genre: "Bebop Jazz"},
                {band: "Make Up",               plays: 57,  genre: "Emo"},
                {band: "Boards of Canada",      plays: 57,  genre: "Electronic"},
                {band: "Grateful Dead",         plays: 46,  genre: "Folk"},
                {band: "Black Ace",             plays: 45,  genre: "Blues"},
                {band: "James Brown",           plays: 44,  genre: "Soul"},
                {band: "Sunny Day in Glasgow",  plays: 41,  genre: "Shoegaze"},
                {band: "Tangerine Dream",       plays: 40,  genre: "Electronic"},
                {band: "Tool",                  plays: 39,  genre: "Metal"},
                {band: "Scientist",             plays: 37,  genre: "Dub"},
                {band: "Dead Can Dance",        plays: 36,  genre: "Darkwave"},
                {band: "Dave Brubeck",          plays: 36,  genre: "Jazz"}];

  Codd.cell = function(table, col, row){ 
    return table[row][col];
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

