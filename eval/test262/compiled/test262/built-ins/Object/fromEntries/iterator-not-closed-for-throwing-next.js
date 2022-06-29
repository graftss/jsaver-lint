function _defineProperty(obj, key, value) { if (key in obj) { Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true }); } else { obj[key] = value; } return obj; }

// Copyright (C) 2018 Kevin Gibbons. All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.

/*---
esid: sec-object.fromentries
description: Does not close iterators with a `next` method which throws.
info: |
  Object.fromEntries ( iterable )

  ...
  4. Let stepsDefine be the algorithm steps defined in CreateDataPropertyOnObject Functions.
  5. Let adder be CreateBuiltinFunction(stepsDefine, « »).
  6. Return ? AddEntriesFromIterable(obj, iterable, adder).

  AddEntriesFromIterable ( target, iterable, adder )

  ...
  4. Repeat,
    a. Let next be ? IteratorStep(iteratorRecord).


  IteratorStep ( iteratorRecord )

  1. Let result be ? IteratorNext(iteratorRecord).


  IteratorNext ( iteratorRecord [ , value ] )

  ...
  3. If Type(result) is not Object, throw a TypeError exception.

features: [Symbol.iterator, Object.fromEntries]
---*/
function DummyError() {}

var iterable = _defineProperty({}, Symbol.iterator, function () {
  return {
    next: function next() {
      throw new DummyError();
    },
    "return": function _return() {
      throw new Test262Error('should not call return');
    }
  };
});

assert["throws"](DummyError, function () {
  Object.fromEntries(iterable);
});