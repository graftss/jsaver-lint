function _typeof(obj) { "@babel/helpers - typeof"; return _typeof = "function" == typeof Symbol && "symbol" == typeof Symbol.iterator ? function (obj) { return typeof obj; } : function (obj) { return obj && "function" == typeof Symbol && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : typeof obj; }, _typeof(obj); }

// Copyright (c) 2012 Ecma International.  All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.

/*---
es5id: 15.2.3.5-4-241
description: >
    Object.create - 'get' property of one property in 'Properties' is
    own accessor property without a get function (8.10.5 step 7.a)
---*/
var descObj = {};
Object.defineProperty(descObj, "get", {
  set: function set() {}
});
var newObj = Object.create({}, {
  prop: descObj
});
assert(newObj.hasOwnProperty("prop"), 'newObj.hasOwnProperty("prop") !== true');
assert.sameValue(_typeof(newObj.prop), "undefined", 'typeof (newObj.prop)');