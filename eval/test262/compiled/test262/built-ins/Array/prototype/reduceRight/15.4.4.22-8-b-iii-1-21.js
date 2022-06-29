// Copyright (c) 2012 Ecma International.  All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.

/*---
esid: sec-array.prototype.reduceright
description: >
    Array.prototype.reduceRight - element to be retrieved is inherited
    accessor property without a get function on an Array-like object
---*/
var testResult = false;

function callbackfn(prevVal, curVal, idx, obj) {
  if (idx === 1) {
    testResult = typeof prevVal === "undefined";
  }
}

var proto = {
  0: 0,
  1: 1
};
Object.defineProperty(proto, "2", {
  set: function set() {},
  configurable: true
});

var Con = function Con() {};

Con.prototype = proto;
var child = new Con();
child.length = 3;
Array.prototype.reduceRight.call(child, callbackfn);
assert(testResult, 'testResult !== true');