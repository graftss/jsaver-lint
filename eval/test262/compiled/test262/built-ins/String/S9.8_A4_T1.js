function _typeof(obj) { "@babel/helpers - typeof"; return _typeof = "function" == typeof Symbol && "symbol" == typeof Symbol.iterator ? function (obj) { return typeof obj; } : function (obj) { return obj && "function" == typeof Symbol && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : typeof obj; }, _typeof(obj); }

// Copyright 2009 the Sputnik authors.  All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.

/*---
info: |
    Result of String conversion from string value is the input argument (no
    conversion)
es5id: 9.8_A4_T1
description: Some strings convert to String with explicit transformation
---*/
// CHECK#1
var x1 = "abc";

if (String(x1) !== x1) {
  $ERROR('#1: String("abc") === "abc". Actual: ' + String("abc"));
} // CHECK#2


var x2 = "abc";

if (_typeof(String(x2)) !== _typeof(x2)) {
  $ERROR('#2: typeof String("abc") === "string". Actual: ' + _typeof(String("abc")));
}