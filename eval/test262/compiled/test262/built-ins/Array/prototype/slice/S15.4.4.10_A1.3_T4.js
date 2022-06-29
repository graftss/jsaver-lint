// Copyright 2009 the Sputnik authors.  All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.

/*---
info: |
    If start is positive, use min(start, length).
    If end is negative, use max(end + length, 0)
esid: sec-array.prototype.slice
description: abs(end) > length > start > 0, end < 0
---*/
var x = [0, 1, 2, 3, 4];
var arr = x.slice(4, -9); //CHECK#1

arr.getClass = Object.prototype.toString;

if (arr.getClass() !== "[object " + "Array" + "]") {
  $ERROR('#1: var x = [0,1,2,3,4]; var arr = x.slice(4,-9); arr is Array object. Actual: ' + arr.getClass());
} //CHECK#2


if (arr.length !== 0) {
  $ERROR('#2: var x = [0,1,2,3,4]; var arr = x.slice(4,-9); arr.length === 0. Actual: ' + arr.length);
} //CHECK#3


if (arr[0] !== undefined) {
  $ERROR('#3: var x = [0,1,2,3,4]; var arr = x.slice(4,-9); arr[0] === undefined. Actual: ' + arr[0]);
}