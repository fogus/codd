//    cod.js
//    http://www.functionaljs.org
//    (c) 2012 Fogus, Ariadne Softworks
//    cod.js may be freely distributed under the MIT license

// *the secret JavaScript sauce*
(function() {
  // Current version
  L.$.VERSION = '0.6.0';

  // Relational algebra

  L.$.toSet = function(array) {
    return L.$.apply(null, array);
  };

  L.$.select = function(xset, pred) {
    return _.reduce(xset, function(result, val) {
      if (L.truthy(pred(val)))
        return result;
      else
        return _.without(result, val);
    }, xset);
  };

  L.$.project = function(table, ks) {
    return _.map(table, function(rel) {
      return _.pick.apply(null, L.cons(rel, ks));
    });
  };

  L.$.rename = function(table, mappings) {
    return _.map(table, L.curry2(L.renameKeys)(mappings));
  };

  L.$.lookup = function(index, obj, alt) {
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

  L.$.put = function(index, key, element) {
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

  L.$.index = function(table, ks) {
    return _.reduce(table, function(index, rel) {
      var ik = L.selectKeys(rel, ks);
      var iv = L.$.lookup(index, ik, L.$());

      return L.$.put(index, ik, rel);
    }, []);
  };

  L.$.naturalJoin = function(table1, table2) {
    var xks = _.keys(_.first(table1));
    var yks = _.keys(_.first(table2));
    var ks  = _.intersection(xks, yks);

    var sz = (_.size(table1) <= _.size(table2)) ? [table1, table2] : [table2, table1];
    var r  = _.first(sz);
    var s  = L.second(sz);

    var idx = L.$.index(r, ks);

    return _.reduce(s, function(result, o){
      var found = L.$.lookup(idx, L.selectKeys(o, ks));

      if (L.existy(found))
        return _.reduce(found, function(agg, rel){
          return L.cons(L.merge(rel, o), agg);
        }, result);
      else
        return result;
    }, []);
  };

  L.$.RQL = {
    select: L.curry2(L.$.select)
  };

}).call(this);

