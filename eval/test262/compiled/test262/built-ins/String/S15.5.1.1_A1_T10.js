function _typeof(obj) { "@babel/helpers - typeof"; return _typeof = "function" == typeof Symbol && "symbol" == typeof Symbol.iterator ? function (obj) { return typeof obj; } : function (obj) { return obj && "function" == typeof Symbol && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : typeof obj; }, _typeof(obj); }

// Copyright 2009 the Sputnik authors.  All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.

/*---
info: |
    When String is called as a function rather than as a constructor, it
    performs a type conversion
es5id: 15.5.1.1_A1_T10
description: Call String(1) and String(-1)
---*/
var __str = String(1); //////////////////////////////////////////////////////////////////////////////
//CHECK#1


if (typeof __str !== "string") {
  $ERROR('#1: __str = String(1); typeof __str === "string". Actual: typeof __str ===' + _typeof(__str));
} //
//////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////
//CHECK#2


if (__str !== "1") {
  $ERROR('#2: __str = String(1); __str === "1". Actual: __str ===' + __str);
} //
//////////////////////////////////////////////////////////////////////////////


__str = String(-1); //////////////////////////////////////////////////////////////////////////////
//CHECK#1

if (typeof __str !== "string") {
  $ERROR('#3: __str = String(-1); typeof __str === "string". Actual: typeof __str ===' + _typeof(__str));
} //
//////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////
//CHECK#2


if (__str !== "-1") {
  $ERROR('#4: __str = String(-1); __str === "-1". Actual: __str ===' + __str);
} //
//////////////////////////////////////////////////////////////////////////////