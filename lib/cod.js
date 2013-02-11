//    cod.js
//    http://www.functionaljs.org
//    (c) 2012 Fogus, Ariadne Softworks
//    cod.js may be freely distributed under the MIT license

// *the secret JavaScript sauce*
(function() {
  // Current version
  L.$.VERSION = '0.5.0';

  // Relational algebra

  L.$.toSet = function(array) {
    return L.$.apply(null, array);
  };

  L.$.select = function(pred, xset) {
    return _.reduce(xset, function(result, val) {
      if (L.truthy(pred(val)))
        return result;
      else
        return _.without(result, val);
    }, xset);
  };

  L.$.project = function(xrel, ks) {
    return _.map(xrel, function(rel) {
      return _.pick.apply(null, L.cons(rel, ks));
    });
  };

  L.$.rename = function(xrel, kobj) {
    return _.map(xrel, L.curry2(L.renameKeys)(kobj));
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

  L.$.index = function(xrel, ks) {
    return _.reduce(xrel, function(index, rel) {
      var ik = L.selectKeys(rel, ks);
      var iv = L.$.lookup(index, ik, L.$());

      return L.$.put(index, ik, rel);
    }, []);
  };

  L.$.naturalJoin = function(xrel, yrel) {
    var xks = _.keys(_.first(xrel));
    var yks = _.keys(_.first(yrel));
    var ks  = _.intersection(xks, yks);

    var sz = (_.size(xrel) <= _.size(yrel)) ? [xrel, yrel] : [yrel, xrel];
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
}).call(this);
