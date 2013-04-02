//    codd.js
//    http://www.functionaljs.org/codd
//    (c) 2013 Fogus, Ariadne Softworks
//    codd.js may be freely distributed under the MIT license

// *the secret JavaScript sauce*
(function() {
  // Imports
  // -------
  var _ = this._ || require('underscore');
  var L = this.L || require('lemonad');

  // The base Codd object
  var Codd = function(array) {
    return L.$.apply(null, array);
  };

  // Current version
  Codd.VERSION = '0.6.5';

  // Relational algebra

  Codd.toSet = Codd;

  Codd.restrict = function(xset, pred) {
    return _.reduce(xset, function(result, val) {
      if (L.truthy(pred(val)))
        return result;
      else
        return _.without(result, val);
    }, xset);
  };

  Codd.project = function(table, ks) {
    return _.map(table, function(rel) {
      return _.pick.apply(null, L.cons(rel, ks));
    });
  };

  Codd.rename = function(table, mappings) {
    return _.map(table, L.curry2(L.renameKeys)(mappings));
  };

  Codd.lookup = function(index, obj, alt) {
    var answer = alt;

    _.some(index, function(entry) {
      var key = _.first(entry);

      if (_.isEqual(key, obj)) {
        answer = _.rest(entry);
        return true;
      }

      return false;
    });

    return answer;
  };

  Codd.put = function(index, key, element) {
    var found = false;
    var result = _.map(index, function(entry) {
      if (_.isEqual(_.first(entry), key)) {
        found = true;
        return L.cat([key, element], _.rest(entry));
      }

      return entry;
    });

    return found ? result : L.cons([key, element], result);
  };

  Codd.index = function(table, ks) {
    return _.reduce(table, function(index, rel) {
      var ik = L.selectKeys(rel, ks);
      var iv = Codd.lookup(index, ik, Codd());

      return Codd.put(index, ik, rel);
    }, []);
  };

  Codd.naturalJoin = function(table1, table2) {
    var xks = _.keys(_.first(table1));
    var yks = _.keys(_.first(table2));
    var ks  = _.intersection(xks, yks);

    var sz = (_.size(table1) <= _.size(table2)) ? [table1, table2] : [table2, table1];
    var r  = _.first(sz);
    var s  = L.second(sz);

    var idx = Codd.index(r, ks);

    return _.reduce(s, function(result, o){
      var found = Codd.lookup(idx, L.selectKeys(o, ks));

      if (L.existy(found))
        return _.reduce(found, function(agg, rel){
          return L.cons(L.merge(rel, o), agg);
        }, result);
      else
        return result;
    }, []);
  };

  Codd.RQL = {
    where:   L.curry2(Codd.restrict),
    select:  L.curry2(Codd.project),
    as:      L.curry2(Codd.rename)
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

