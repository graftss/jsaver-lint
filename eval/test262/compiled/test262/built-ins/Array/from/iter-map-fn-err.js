// Copyright (C) 2015 the V8 project authors. All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.

/*---
esid: sec-array.from
description: Error invoking map function (traversed via iterator)
info: |
    [...]
    6. If usingIterator is not undefined, then
       [...]
       g. Repeat
          [...]
          vii. If mapping is true, then
               1. Let mappedValue be Call(mapfn, T, «nextValue, k»).
               2. If mappedValue is an abrupt completion, return
                  IteratorClose(iterator, mappedValue).
features: [Symbol.iterator]
---*/
var closeCount = 0;

var mapFn = function mapFn() {
  throw new Test262Error();
};

var items = {};

items[Symbol.iterator] = function () {
  return {
    "return": function _return() {
      closeCount += 1;
    },
    next: function next() {
      return {
        done: false
      };
    }
  };
};

assert["throws"](Test262Error, function () {
  Array.from(items, mapFn);
});
assert.sameValue(closeCount, 1);