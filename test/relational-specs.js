describe("Relational algebra functions", function() {
  describe("Codd", function() {
    it("should return an array of uniq elements", function() {
      var s = L.$(1,2,3,4,5,3,4,5);

      expect(s.constructor).toBe(Array);
      expect(s).toEqual([1,2,3,4,5]);
    });
  });

  describe("Codd.set", function() {
    it("should return an array of uniq elements", function() {
      var s = Codd.toSet([1,2,3,4,5,3,4,5,8,2,3]);

      expect(s.constructor).toBe(Array);
      expect(s).toEqual([1,2,3,4,5,8]);
    });
  });

  describe("Codd.restrict", function() {
    it("should return an array of uniq elements matching a predicate", function() {
      var s = L.$(1,2,3,4,5,3,4,5);
      var result = Codd.restrict(s, L.isOdd);

      expect(result.constructor).toBe(Array);
      expect(result).toEqual([1,3,5]);
    });
  });

  describe("Codd.RQL.where", function() {
    it("should return an array of uniq elements matching a predicate (curried)", function() {
      var s = L.$(1,2,3,4,5,3,4,5);
      var result = Codd.RQL.where(L.isOdd)(s);

      expect(result.constructor).toBe(Array);
      expect(result).toEqual([1,3,5]);
    });
  });

  describe("Codd.project", function() {
    it("should return an array of objects with only the keys wanted", function() {
      var s = L.$({a: 1, b: 2}, {a: 2, b: 4});
      var result = Codd.project(s, ['a']);

      expect(result.constructor).toBe(Array);
      expect(result).toEqual([{a: 1}, {a: 2}]);
    });
  });

  describe("Codd.RQL.select", function() {
    it("should return an array of objects with only the keys wanted (curried)", function() {
      var s = L.$({a: 1, b: 2}, {a: 2, b: 4});
      var result = Codd.RQL.select(['a'])(s);

      expect(result.constructor).toBe(Array);
      expect(result).toEqual([{a: 1}, {a: 2}]);
    });
  });

  describe("Codd.rename", function() {
    it("should rename the keys in an array of objects with according to a mapping in the given object", function() {
      var s  = [{'a': 1, 'b': 2}, {'a': 3}, {'b': 4}];
      var st = [{'AAA': 1, 'b': 2}, {'AAA': 3}, {'b': 4}];
      var result = Codd.rename(s, {'a': 'AAA'});

      expect(result.constructor).toBe(Array);
      expect(result).toEqual(st);
    });

    it("should not modify the original array of objects", function() {
      var s  = [{'a': 1, 'b': 2}, {'a': 3}, {'b': 4}];
      var target = [{'a': 1, 'b': 2}, {'a': 3}, {'b': 4}];
      var _ = Codd.rename(s, {'a': 'AAA'});

      expect(s).toEqual(target);
    });
  });
/*
  describe("Codd.RQL.as", function() {
    it("should rename the keys in an array of objects with according to a mapping in the given object (curried)", function() {
      var s  = [{'a': 1, 'b': 2}, {'a': 3}, {'b': 4}];
      var st = [{'AAA': 1, 'b': 2}, {'AAA': 3}, {'b': 4}];
      var result = Codd.RQL.as({'a': 'AAA'})(s);

      expect(result.constructor).toBe(Array);
      expect(result).toEqual(st);
    });

    it("should not modify the original array of objects", function() {
      var s  = [{'a': 1, 'b': 2}, {'a': 3}, {'b': 4}];
      var target = [{'a': 1, 'b': 2}, {'a': 3}, {'b': 4}];
      var _ = Codd.RQL.as({'a': 'AAA'})(s);

      expect(s).toEqual(target);
    });
  });

  describe("Codd.lookup", function() {
    it("should return an array of objects matching the key given in the index", function() {
      var testIndex = [[{a: 1}, {name: 'foo', a: 1}, {name: 'bar', a: 1}], [{a: 2}, {a: 2, name: 'baz'}]];
      var result1   = Codd.lookup(testIndex, {a: 1});
      var result2   = Codd.lookup(testIndex, {a: 2});
      var resultNo  = Codd.lookup(testIndex, {a: 'not there'});

      expect(result1.constructor).toBe(Array);
      expect(result1).toEqual([{a: 1, name: 'foo'}, {a: 1, name: 'bar'}]);

      expect(result2.constructor).toBe(Array);
      expect(result2).toEqual([{a: 2, name: 'baz'}]);

      expect(resultNo).toBe(undefined);
    });
  });

  describe("Codd.put", function() {
    it("should return an index with a new entry, even when the index is empty", function() {
      var testIndexEmpty = [];

      expect(Codd.put(testIndexEmpty, {a: 1}, 42)).toEqual([[{a: 1}, 42]]);
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
*/
});
