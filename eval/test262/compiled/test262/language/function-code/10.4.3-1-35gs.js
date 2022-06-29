function _typeof(obj) { "@babel/helpers - typeof"; return _typeof = "function" == typeof Symbol && "symbol" == typeof Symbol.iterator ? function (obj) { return typeof obj; } : function (obj) { return obj && "function" == typeof Symbol && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : typeof obj; }, _typeof(obj); }

// Copyright (c) 2012 Ecma International.  All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.

/*---
es5id: 10.4.3-1-35gs
description: >
    Strict - checking 'this' from a global scope (Anonymous
    FunctionExpression defined within an Anonymous FunctionExpression
    inside strict mode)
flags: [onlyStrict]
---*/
if (!function () {
  return function () {
    return _typeof(this);
  }() === "undefined" && typeof this === "undefined";
}()) {
  throw "'this' had incorrect value!";
}