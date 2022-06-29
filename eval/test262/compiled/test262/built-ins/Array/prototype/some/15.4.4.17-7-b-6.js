// Copyright (c) 2012 Ecma International.  All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.

/*---
esid: sec-array.prototype.some
description: >
    Array.prototype.some - properties can be added to prototype after
    current position are visited on an Array-like object
---*/
function callbackfn(val, idx, obj) {
  if (idx === 1 && val === 6.99) {
    return true;
  } else {
    return false;
  }
}

var arr = {
  length: 2
};
Object.defineProperty(arr, "0", {
  get: function get() {
    Object.defineProperty(Object.prototype, "1", {
      get: function get() {
        return 6.99;
      },
      configurable: true
    });
    return 0;
  },
  configurable: true
});
assert(Array.prototype.some.call(arr, callbackfn), 'Array.prototype.some.call(arr, callbackfn) !== true');