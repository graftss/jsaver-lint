// Copyright 2009 the Sputnik authors.  All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.

/*---
info: Operator x / y returns ToNumber(x) / ToNumber(y)
es5id: 11.5.2_A3_T2.7
description: >
    Type(x) is different from Type(y) and both types vary between
    String (primitive or object) and Null
---*/
//CHECK#1
if ("1" / null !== Number.POSITIVE_INFINITY) {
  $ERROR('#1: "1" / null === +Infinity. Actual: ' + "1" / null);
} //CHECK#2


if (null / "1" !== 0) {
  $ERROR('#2: null / "1" === 0. Actual: ' + null / "1");
} //CHECK#3


if (new String("1") / null !== Number.POSITIVE_INFINITY) {
  $ERROR('#3: new String("1") / null === +Infinity. Actual: ' + new String("1") / null);
} //CHECK#4


if (null / new String("1") !== 0) {
  $ERROR('#4: null / new String("1") === 0. Actual: ' + null / new String("1"));
}