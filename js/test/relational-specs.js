const Codd = require('../lib/codd');
const L = require('lemonad');

describe("Relational algebra functions - Boundary Conditions", function() {
  describe("Codd.toSet", function() {
    it("should return an empty array when input is empty", function() {
      expect(Codd.toSet([])).toEqual([]);
    });

    it("should handle array with all duplicates", function() {
      expect(Codd.toSet([1,1,1,1])).toEqual([1]);
    });

    it("should handle array with different types", function() {
      expect(Codd.toSet([1, '1', 1])).toEqual([1, '1']);
    });
  });

  describe("Codd.difference", function() {
    it("should return empty array if both sets are empty", function() {
      expect(Codd.difference([])([])).toEqual([]);
    });

    it("should return L if R is empty", function() {
      expect(Codd.difference([])([1,2,3])).toEqual([1,2,3]);
    });

    it("should return empty if L is empty", function() {
      expect(Codd.difference([1,2,3])([])).toEqual([]);
    });
  });

  describe("Codd.intersection", function() {
    it("should return empty array if both sets are empty", function() {
      expect(Codd.intersection([])([])).toEqual([]);
    });

    it("should return empty array if no intersection", function() {
      expect(Codd.intersection([4,5])([1,2,3])).toEqual([]);
    });

    it("should handle intersection with one element", function() {
      expect(Codd.intersection([2,3])([3,4])).toEqual([3]);
    });
  });

  describe("Codd.union", function() {
    it("should return empty array if both sets are empty", function() {
      expect(Codd.union([])([])).toEqual([]);
    });

    it("should return L if R is empty", function() {
      expect(Codd.union([])([1,2,3])).toEqual([1,2,3]);
    });

    it("should return R if L is empty", function() {
      expect(Codd.union([1,2,3])([])).toEqual([1,2,3]);
    });
  });

  describe("Codd.restrict", function() {
    it("should return empty array if no elements match predicate", function() {
      var s = L.$(2,4,6);
      var result = Codd.restrict(s, L.isOdd);
      expect(result).toEqual([]);
    });

    it("should return all elements if all match predicate", function() {
      var s = L.$(1,3,5);
      var result = Codd.restrict(s, L.isOdd);
      expect(result).toEqual([1,3,5]);
    });

    it("should return empty array for empty input", function() {
      expect(Codd.restrict([], L.isOdd)).toEqual([]);
    });
  });

  describe("Codd.project", function() {
    it("should return empty array for empty input", function() {
      expect(Codd.project([], ['a'])).toEqual([]);
    });

    it("should return array of empty objects if keys not present", function() {
      var s = L.$({b: 2}, {b: 4});
      expect(Codd.project(s, ['a'])).toEqual([{},{ }]);
    });

    it("should return original objects if all keys are projected", function() {
      var s = L.$({a: 1, b: 2});
      expect(Codd.project(s, ['a', 'b'])).toEqual([{a: 1, b: 2}]);
    });
  });

  describe("Codd.rename", function() {
    it("should return empty array for empty input", function() {
      expect(Codd.rename([], {'a': 'AAA'})).toEqual([]);
    });

    it("should not rename if mapping is empty", function() {
      var s = [{'a': 1, 'b': 2}];
      expect(Codd.rename(s, {})).toEqual([{'a': 1, 'b': 2}]);
    });

    it("should not rename keys not present in mapping", function() {
      var s = [{'a': 1, 'b': 2}];
      expect(Codd.rename(s, {'c': 'C'})).toEqual([{'a': 1, 'b': 2}]);
    });
  });

  describe("Codd.lookup", function() {
    it("should return undefined for empty index", function() {
      expect(Codd.lookup([], {a: 1})).toBe(undefined);
    });

    it("should return undefined if key not found", function() {
      var testIndex = [[{a: 1}, {name: 'foo', a: 1}]];
      expect(Codd.lookup(testIndex, {a: 2})).toBe(undefined);
    });

    it("should handle index with empty buckets", function() {
      var testIndex = [[{a: 1}], []];
      expect(Codd.lookup(testIndex, {a: 1})).toEqual([]);
    });
  });

  describe("Codd.put", function() {
    it("should add to empty index", function() {
      expect(Codd.put([], {a: 1}, 99)).toEqual([[{a: 1}, 99]]);
    });

    it("should return an index with a new entry", function() {
      var testIndex = [[{a: 1}, {name: 'foo', a: 1}, {name: 'bar', a: 1}], [{a: 2}, {a: 2, name: 'baz'}]];
      var exp = [[{a: 1}, 42, {name: 'foo', a: 1}, {name: 'bar', a: 1}], [{a: 2}, {a: 2, name: 'baz'}]];

      expect(Codd.put(testIndex, {a: 1}, 42)).toEqual(exp);
    });
  });  

  describe("Codd.index", function() {
    it("should return an index of the objects based on the keys given", function() {
      var db = L.$({name: 'Burial', genre: 'dubstep'},
                   {name: 'Donovan', genre: 'folk'},
                   {name: 'Ikonika', genre: 'dubstep'});

      var index = Codd.index(db, ['genre']);

      var folk = Codd.lookup(index, {genre: 'folk'});

      expect(index[0].length).toBe(2);
      expect(index[1].length).toBe(3);
      expect(folk).toEqual([{name: 'Donovan', genre: 'folk'}]);
    });
  });
});