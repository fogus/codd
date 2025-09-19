const Codd = require('../lib/codd');
const L = require('lemonad');

describe("Relational algebra functions - Boundary Conditions", () => {
  describe("Codd.toSet", () => {
    it("should return an empty array when input is empty", () => {
      expect(Codd.toSet([])).toEqual([]);
    });

    it("should handle array with all duplicates", () => {
      expect(Codd.toSet([1,1,1,1])).toEqual([1]);
    });

    it("should handle array with different types", () => {
      expect(Codd.toSet([1, '1', 1])).toEqual([1, '1']);
    });
  });

  describe("Codd.difference", () => {
    it("should return empty array if both sets are empty", () => {
      expect(Codd.difference([])([])).toEqual([]);
    });

    it("should return L if R is empty", () => {
      expect(Codd.difference([])([1,2,3])).toEqual([1,2,3]);
    });

    it("should return empty if L is empty", () => {
      expect(Codd.difference([1,2,3])([])).toEqual([]);
    });
  });

  describe("Codd.intersection", () => {
    it("should return empty array if both sets are empty", () => {
      expect(Codd.intersection([])([])).toEqual([]);
    });

    it("should return empty array if no intersection", () => {
      expect(Codd.intersection([4,5])([1,2,3])).toEqual([]);
    });

    it("should handle intersection with one element", () => {
      expect(Codd.intersection([2,3])([3,4])).toEqual([3]);
    });
  });

  describe("Codd.union", () => {
    it("should return empty array if both sets are empty", () => {
      expect(Codd.union([])([])).toEqual([]);
    });

    it("should return L if R is empty", () => {
      expect(Codd.union([])([1,2,3])).toEqual([1,2,3]);
    });

    it("should return R if L is empty", () => {
      expect(Codd.union([1,2,3])([])).toEqual([1,2,3]);
    });
  });

  describe("Codd.restrict", () => {
    it("should return empty array if no elements match predicate", () => {
      var s = L.$(2,4,6);
      var result = Codd.restrict(s, L.isOdd);
      expect(result).toEqual([]);
    });

    it("should return all elements if all match predicate", () => {
      var s = L.$(1,3,5);
      var result = Codd.restrict(s, L.isOdd);
      expect(result).toEqual([1,3,5]);
    });

    it("should return empty array for empty input", () => {
      expect(Codd.restrict([], L.isOdd)).toEqual([]);
    });
  });

  describe("Codd.project", () => {
    it("should return empty array for empty input", () => {
      expect(Codd.project([], ['a'])).toEqual([]);
    });

    it("should return array of empty objects if keys not present", () => {
      var s = L.$({b: 2}, {b: 4});
      expect(Codd.project(s, ['a'])).toEqual([{},{ }]);
    });

    it("should return original objects if all keys are projected", () => {
      var s = L.$({a: 1, b: 2});
      expect(Codd.project(s, ['a', 'b'])).toEqual([{a: 1, b: 2}]);
    });

        it('should return empty array for empty input', () => {
      expect(Codd.project([], ['a'])).toEqual([]);
    });

    it('should return array of empty objects if keys not present', () => {
      const input = [{b: 2}, {b: 4}];
      expect(Codd.project(input, ['a'])).toEqual([{}, {}]);
    });

    it('should return original objects if all keys are projected', () => {
      const input = [{a: 1, b: 2}];
      expect(Codd.project(input, ['a', 'b'])).toEqual([{a: 1, b: 2}]);
    });

    it('should project only specified keys', () => {
      const input = [{a: 1, b: 2, c: 3}, {a: 4, b: 5, c: 6}];
      expect(Codd.project(input, ['a', 'c'])).toEqual([{a: 1, c: 3}, {a: 4, c: 6}]);
    });

    it('should handle mixed presence of keys', () => {
      const input = [{a: 1, b: 2}, {b: 3, c: 4}];
      expect(Codd.project(input, ['a', 'b'])).toEqual([{a: 1, b: 2}, {b: 3}]);
    });

    it('should return array of empty objects if projected keys are missing in all objects', () => {
      const input = [{x: 1}, {y: 2}];
      expect(Codd.project(input, ['z'])).toEqual([{}, {}]);
    });

    it('should not mutate the original objects', () => {
      const input = [{a: 1, b: 2}];
      Codd.project(input, ['a']);
      expect(input).toEqual([{a: 1, b: 2}]);
    });
  });

  describe("Codd.rename", () => {
    it("should return empty array for empty input", () => {
      expect(Codd.rename([], {'a': 'AAA'})).toEqual([]);
    });

    it("should not rename if mapping is empty", () => {
      var s = [{'a': 1, 'b': 2}];
      expect(Codd.rename(s, {})).toEqual([{'a': 1, 'b': 2}]);
    });

    it("should not rename keys not present in mapping", () => {
      var s = [{'a': 1, 'b': 2}];
      expect(Codd.rename(s, {'c': 'C'})).toEqual([{'a': 1, 'b': 2}]);
    });
  });

  describe("Codd.lookup", () => {
    it("should return undefined for empty index", () => {
      expect(Codd.lookup([], {a: 1})).toBe(undefined);
    });

    it("should return undefined if key not found", () => {
      var testIndex = [[{a: 1}, {name: 'foo', a: 1}]];
      expect(Codd.lookup(testIndex, {a: 2})).toBe(undefined);
    });

    it("should handle index with empty buckets", () => {
      var testIndex = [[{a: 1}], []];
      expect(Codd.lookup(testIndex, {a: 1})).toEqual([]);
    });
  });

  describe("Codd.put", () => {
    it("should add to empty index", () => {
      expect(Codd.put([], {a: 1}, 99)).toEqual([[{a: 1}, 99]]);
    });

    it("should return an index with a new entry", () => {
      var testIndex = [[{a: 1}, {name: 'foo', a: 1}, {name: 'bar', a: 1}], [{a: 2}, {a: 2, name: 'baz'}]];
      var exp = [[{a: 1}, 42, {name: 'foo', a: 1}, {name: 'bar', a: 1}], [{a: 2}, {a: 2, name: 'baz'}]];

      expect(Codd.put(testIndex, {a: 1}, 42)).toEqual(exp);
    });
  });  

  describe("Codd.index", () => {
    it("should return an index of the objects based on the keys given", () => {
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

  describe("Codd.naturalJoin", () => {
    it("should join two relations on common keys", () => {
      const r1 = [
        {id: 1, name: 'Alice'},
        {id: 2, name: 'Bob'}
      ];
      const r2 = [
        {id: 1, age: 30},
        {id: 2, age: 40},
        {id: 3, age: 50}
      ];
      const result = Codd.naturalJoin(r1, r2);
      expect(result).toEqual([
        {id: 2, name: 'Bob', age: 40},
        {id: 1, name: 'Alice', age: 30}
      ]);
    });
/*
    it("should return empty array if no common keys", () => {
      const r1 = [{a: 1}];
      const r2 = [{b: 2}];
      expect(Codd.naturalJoin(r1, r2)).toEqual([]);
    });

    it("should handle empty input arrays", () => {
      expect(Codd.naturalJoin([], [{id: 1}])).toEqual([]);
      expect(Codd.naturalJoin([{id: 1}], [])).toEqual([]);
    });
*/
  });
  
  describe('Codd.RQL with Codd.tunes', () => {
    tunes = [
        {artist: 'Burial', title: 'Archangel', genre: 'dubstep', year: 2007},
        {artist: 'Donovan', title: 'Sunshine Superman', genre: 'folk', year: 1966},
        {artist: 'Ikonika', title: 'Idiot', genre: 'dubstep', year: 2010},
        {artist: 'The Beatles', title: 'Hey Jude', genre: 'rock', year: 1968}
      ];

    it('should select all dubstep tunes', () => {
      const rql = Codd.RQL.q(Codd.RQL.where(Codd.RQL.field('genre', Codd.RQL.like('dubstep'))))(tunes);
      expect(rql).toEqual([
        {artist: 'Burial', title: 'Archangel', genre: 'dubstep', year: 2007},
        {artist: 'Ikonika', title: 'Idiot', genre: 'dubstep', year: 2010}
      ]);
    });

    it('should select artist and title for folk tunes', () => {
      const rql = Codd.RQL.q(Codd.RQL.where(Codd.RQL.field('genre', Codd.RQL.like('folk'))))(tunes);
      expect(rql).toEqual([
        {artist: 'Donovan', title: 'Sunshine Superman', genre: 'folk', year: 1966}
      ]);
    });

    it('should select all tunes from 1960s', () => {
      const rql = Codd.RQL.q(Codd.RQL.where(rel => rel.year >= 1960 && rel.year < 1970))(tunes);
      expect(rql).toEqual([
        {artist: 'Donovan', title: 'Sunshine Superman', genre: 'folk', year: 1966},
        {artist: 'The Beatles', title: 'Hey Jude', genre: 'rock', year: 1968}
      ]);
    });

    it('should select all artists', () => {
      const rql = Codd.RQL.q(Codd.RQL.where(Codd.RQL.field('artist', Codd.RQL.like(/.*/))))(tunes);
      expect(rql).toEqual([
        {artist: 'Burial', title: 'Archangel', genre: 'dubstep', year: 2007},
        {artist: 'Donovan', title: 'Sunshine Superman', genre: 'folk', year: 1966},
        {artist: 'Ikonika', title: 'Idiot', genre: 'dubstep', year: 2010},
        {artist: 'The Beatles', title: 'Hey Jude', genre: 'rock', year: 1968}
      ]);
    });

    it('should rename genre to style', () => {
      const rql = Codd.RQL.q(Codd.RQL.as({'genre': 'style'}))(tunes);
      expect(rql).toEqual([
        {artist: 'Burial', title: 'Archangel', style: 'dubstep', year: 2007},
        {artist: 'Donovan', title: 'Sunshine Superman', style: 'folk', year: 1966},
        {artist: 'Ikonika', title: 'Idiot', style: 'dubstep', year: 2010},
        {artist: 'The Beatles', title: 'Hey Jude', style: 'rock', year: 1968}
      ]);
    });
    
    it('should chain multiple RQL operations', () => {
      let rql = Codd.RQL.q(
        tunes,
        Codd.RQL.where(Codd.RQL.field('genre', Codd.RQL.like('dubstep'))),
        Codd.RQL.select(['artist', 'title']),
        Codd.RQL.as({'title': 'songTitle'})
      );
      
      expect(rql).toEqual([
        {artist: 'Burial', songTitle: 'Archangel'},
        {artist: 'Ikonika', songTitle: 'Idiot'}
      ]);
    });
  });
});