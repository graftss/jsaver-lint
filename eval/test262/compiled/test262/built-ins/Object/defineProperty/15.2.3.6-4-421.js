// Copyright (c) 2012 Ecma International.  All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.

/*---
es5id: 15.2.3.6-4-421
description: >
    ES5 Attributes - Inherited property whose [[Enumerable]] attribute
    is set to false is non-enumerable (Function.prototype.bind)
---*/
var foo = function foo() {};

Object.defineProperty(Function.prototype, "prop", {
  value: 1001,
  writable: false,
  enumerable: false,
  configurable: true
});
var obj = foo.bind({});
var verifyEnumerable = false;

for (var p in obj) {
  if (p === "prop") {
    verifyEnumerable = true;
  }
}

assert.sameValue(obj.hasOwnProperty("prop"), false, 'obj.hasOwnProperty("prop")');
assert.sameValue(verifyEnumerable, false, 'verifyEnumerable');