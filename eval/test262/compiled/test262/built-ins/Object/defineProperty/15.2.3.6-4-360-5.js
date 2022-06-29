function _typeof(obj) { "@babel/helpers - typeof"; return _typeof = "function" == typeof Symbol && "symbol" == typeof Symbol.iterator ? function (obj) { return typeof obj; } : function (obj) { return obj && "function" == typeof Symbol && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : typeof obj; }, _typeof(obj); }

// Copyright (c) 2012 Ecma International.  All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.

/*---
es5id: 15.2.3.6-4-360-5
description: >
    ES5 Attributes - Updating named data property 'P' whose attributes
    are [[Writable]]: false, [[Enumerable]]: true, [[Configurable]]:
    true to an accessor property, 'A' is an Array object (8.12.9 -
    step 9.b.i)
---*/
var obj = [];
Object.defineProperty(obj, "prop", {
  value: 2010,
  writable: false,
  enumerable: true,
  configurable: true
});
var desc1 = Object.getOwnPropertyDescriptor(obj, "prop");

function getFunc() {
  return 20;
}

Object.defineProperty(obj, "prop", {
  get: getFunc
});
var desc2 = Object.getOwnPropertyDescriptor(obj, "prop");
assert(desc1.hasOwnProperty("value"), 'desc1.hasOwnProperty("value") !== true');
assert(desc2.hasOwnProperty("get"), 'desc2.hasOwnProperty("get") !== true');
assert.sameValue(desc2.enumerable, true, 'desc2.enumerable');
assert.sameValue(desc2.configurable, true, 'desc2.configurable');
assert.sameValue(obj.prop, 20, 'obj.prop');
assert.sameValue(_typeof(desc2.set), "undefined", 'typeof desc2.set');
assert.sameValue(desc2.get, getFunc, 'desc2.get');