// First class functions

hello();

hello;

function callThis(f: Function) {
  return f();
}

callThis(hello);

// Functor

function map(func: (a: any) => any, list: any[]): any[] {
    var new_list = [], i;
    for (i = 0; i < list.length; i++) {
        new_list.push(func(list[i]));
    }
    return new_list;
}

function filter(func: (any) => bool, list: any[]) {
    var new_list = [], i;
    for (i = 0; i < list.length; i++) {
        if (func(list[i])) {
            new_list.push(list[i]);
        }
    }
    return new_list;
}

// Reducers

function reduce(func: (a, b) => any, list: any[], initial) {
    var result = initial, i: number;
    for (i = 0; i < list.length; i++) {
        result = func(result, list[i]);
    }
    return result;
}

function shoutyMapReducer(a: string[], b: string): string[] {
    return a.concat([b.toUpperCase()]);
}

// Higher order functions

function mapReducer(func: (a) => any) {
    return function(a, b) {
        return a.concat(func(b));
    }
}

// Combinators

function andThree(func: (a: number) => number) {
    return function(x: number) {
        return func(x) + 3;
    }
}

function nullCheck(func: (x: any) => any) {
    return function(x) {
        if (x === null) return null;
        else return func(x);
    }
}

// Composition

function compose(func1: (a) => any, func2: (a) => any) {
    return function(x: any) {
        return func2(func1(x));
    }
}

// Applicative functors

amap([hi, CAPS], ponies);

function hug(p1: string, p2: string) {
    return p1 + " hugs " + p2;
}

amap([hug], ponies, ponies);

// Currying

add(1, 2, 3);

curry(add, 3)(1)(2)(3);

var onePlusTwoPlus = curry(add, 3)(1)(2);
onePlusTwoPlus(3);

// Partial application

partial(add, 1, 2)(3);

// Monads

compose(children, children)(ponies);

function unit(value: any): any[] {
    return [value];
}

function bind(func: (value: any) => any[]): (list: any[]) => any[] {
    return function(list: any[]) {
        var out = [], i;
        for (i = 0; i < list.length; i++) {
            out = out.concat(func(list[i]));
        }
        return out;
    }
}

compose(bind(children), bind(children), bind(children))(unit(ponies));
