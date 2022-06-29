// Copyright 2009 the Sputnik authors.  All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.

/*---
info: Operator x @= y returns x @ y
es5id: 11.13.2_A3.2_T5
description: Checking Expression and Variable statements for x -= y
---*/
//CHECK#1
var x = -1;
var x1 = x -= 1;

if (x1 !== -2) {
  $ERROR('#1: var x = -1; var x1 = (x -= 1); x1 === -2. Actual: ' + x1);
} //CHECK#2


var y;
var y1;
y = -1;
y1 = y -= 1;

if (y1 !== -2) {
  $ERROR('#2: y = -1; y1 = (y -= 1); y1 === -2. Actual: ' + y1);
}