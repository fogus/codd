
//    codd.js
//    http://www.functionaljs.org/codd
//    (c) 2013-2025 Fogus
//    codd.js may be freely distributed under the MIT license

// *the secret JavaScript sauce*
(function() {
  // Imports
  // -------
  const L = this.L || require('lemonad');

  // The base Codd object
  const Codd = function(array) {
    // TODO: Implement more efficiently
    const set = {};
    const results = [];

    for (const index in array) {
      set[JSON.stringify(array[index])] = array[index];
    }

    for (const key in set) {
      results.push(set[key]);
    }

    return results;
  };

  // Current version
  Codd.VERSION = '0.8.3';

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
    return (rhs) => L.filter(elem => rhs.indexOf(elem) != -1, lhs);
  };

  // TODO: Reimplement more efficiently
  Codd.union = function(lhs) {
    return (rhs) => L.uniq(L.cat(lhs, rhs));
  };

  // TODO: Reimplement more efficiently
  Codd.difference = function(minus) {
    return (target) => L.filter(value => !L.existy(L.has(L.eq(value))(minus)), target);
  };

  Codd.restrict = function(xset, pred) {
    return L.reduce((result, val) => {
      if (L.truthy(pred(val)))
        return result;
      else
        return Codd.difference([val])(result);
    }, xset, xset);
  };

  Codd.selectKeys = (keys) => (rel) => {
      const copy = {};

      for (const key in keys) {
        if (keys[key] in rel) copy[keys[key]] = rel[keys[key]];
      };

      return copy;
    };

  Codd.project = (table, ks) => L.map(key => {
      const rel = table[key];
      return Codd.selectKeys(ks)(rel);
    }, Codd.keys(table));

  Codd.omitKeys = (minus) => (rel) => {
      const copy = {};
      let value;

      for (const key in rel) {
        if (!L.existy(L.has(L.eq(key))(minus)))
          copy[key] = rel[key];
      }

      return copy;
    };

  Codd.renameKeys = (obj, kobj) => {
    const seed = Codd.omitKeys(Codd.keys(kobj))(obj);

    return L.reduce((o, key) => {
      if (L.existy(obj[key])) o[kobj[key]] = obj[key];

      return o;
    },
    seed,
    Codd.keys(kobj));
  };

  Codd.rename = (table, mappings) => L.map(L.rcurry2(Codd.renameKeys)(mappings), table);

  Codd.lookup = (index, obj, alt) => {
    let answer = alt;

    L.has(entry => {
      const key = L.first(entry);

      if (L.eq(key)(obj)) {
        answer = L.tail(entry);
        return true;
      }

      return false;
    })(index);

    return answer;
  };

  Codd.put = (index, key, element) => {
    let found = false;
    const result = L.map(entry => {
      if (L.eq(L.first(entry))(key)) {
        found = true;
        return L.cat([key, element], L.tail(entry));
      }

      return entry;
    }, index);

    return found ? result : L.cons([key, element], result);
  };

  Codd.index = (table, ks) => L.reduce((index, rel) => {
      const ik = Codd.selectKeys(ks)(rel);
      const iv = Codd.lookup(index, ik, Codd());

      return Codd.put(index, ik, rel);
    }, [], table);

  Codd.naturalJoin = (table1, table2) => {
    const xks = Codd.keys(L.first(table1));
    const yks = Codd.keys(L.first(table2));
    const ks  = Codd.intersection(xks)(yks);

    const sz = (L.len(table1) <= L.len(table2)) ? [table1, table2] : [table2, table1];
    const r  = sz[0];
    const s  = sz[1];

    const idx = Codd.index(r, ks);

    return L.reduce((result, o) => {
      const found = Codd.lookup(idx, Codd.selectKeys(ks)(o));

      if (L.existy(found))
        return L.reduce((agg, rel) => L.cons(L.merge(rel, o), agg), result, found);
      else
        return result;
    }, [], s);
  };

  const _like = (r) => (rel, field) => {
      const regex = (r instanceof RegExp) ? r : new RegExp(r);
      return !! rel[field].match(regex)
    };

  const _field = (field, matcher) => {
  const f = L.existy(matcher) ? matcher : ((rel, field) => rel[field]);
  return (rel) => f(rel, field);
  };

  Codd.RQL = {
    where:   L.rcurry2(Codd.restrict),
    select:  L.rcurry2(Codd.project),
    as:      L.rcurry2(Codd.rename),
    like:    _like,
    field:   _field,
    q:       L.pipeline
  };

  Codd.tunes = [{band: "Blind Lemon Jefferson", plays: 141, genre: "Delta Blues"},
                {band: "Erroll Garner",         plays: 115, genre: "Jazz Piano"},
                {band: "Brian Eno",             plays: 94,  genre: "Electronic"},
                {band: "Charlie Parker",        plays: 75,  genre: "Bebop Jazz"},
                {band: "Alice Coltrane",        plays: 73,  genre: "Free Jazz"},
                {band: "Tim Hecker",            plays: 70,  genre: "Ambient"},
                {band: "Charles Mingus",        plays: 69,  genre: "Bebop Jazz"},
                {band: "Bjork",                 plays: 57,  genre: "Electronic"},
                {band: "Boards of Canada",      plays: 57,  genre: "Electronic"},
                {band: "Billy Holliday",        plays: 46,  genre: "Jazz"},
                {band: "Black Ace",             plays: 45,  genre: "Texas Blues"},
                {band: "James Brown",           plays: 44,  genre: "Soul"},
                {band: "Sunny Day in Glasgow",  plays: 41,  genre: "Shoegaze"},
                {band: "Tangerine Dream",       plays: 40,  genre: "Electronic"},
                {band: "Tool",                  plays: 39,  genre: "Metal"},
                {band: "Scientist",             plays: 37,  genre: "Dub"},
                {band: "Dead Can Dance",        plays: 36,  genre: "Darkwave"},
                {band: "Dave Brubeck",          plays: 36,  genre: "Jazz"}];

  Codd.cell = (table, col, row) => table[row][col];

  Codd.col = (table, tag) => {
  if (!L.existy(tag)) return (t) => L.map(Codd.RQL.field(table), t);

  return L.map(Codd.RQL.field(tag), table);
  };

  Codd.row = L.rot(L.uncurry(L.nth));
 
  // Exports
  // -------

  const root = this;

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

