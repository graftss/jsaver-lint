function _typeof(obj) { "@babel/helpers - typeof"; return _typeof = "function" == typeof Symbol && "symbol" == typeof Symbol.iterator ? function (obj) { return typeof obj; } : function (obj) { return obj && "function" == typeof Symbol && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : typeof obj; }, _typeof(obj); }

// Copyright (c) 2012 Ecma International.  All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.

/*---
es5id: 15.2.3.6-4-431
description: >
    ES5 Attributes - [[Get]] attribute of accessor property ([[Get]]
    is undefined, [[Set]] is undefined, [[Enumerable]] is true,
    [[Configurable]] is false) is undefined
---*/
var obj = {};
Object.defineProperty(obj, "prop", {
  get: undefined,
  set: undefined,
  enumerable: true,
  configurable: false
});
var propertyDefineCorrect = obj.hasOwnProperty("prop");
var desc = Object.getOwnPropertyDescriptor(obj, "prop");
assert(propertyDefineCorrect, 'propertyDefineCorrect !== true');
assert.sameValue(_typeof(desc.get), "undefined", 'typeof desc.get');