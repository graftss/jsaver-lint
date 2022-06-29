// Copyright (c) 2012 Ecma International.  All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.

/*---
es5id: 15.2.3.6-4-271
description: >
    Object.defineProperty - 'O' is an Array, 'name' is an array index
    named property, name is accessor property and 'desc' is accessor
    descriptor, test updating the [[Enumerable]] attribute value of
    'name' (15.4.5.1 step 4.c)
includes: [propertyHelper.js]
---*/
var arrObj = [];

function setFunc(value) {
  arrObj.setVerifyHelpProp = value;
}

Object.defineProperty(arrObj, "0", {
  set: setFunc,
  enumerable: true,
  configurable: true
});
Object.defineProperty(arrObj, "0", {
  enumerable: false
});
verifyWritable(arrObj, "0", "setVerifyHelpProp");
verifyNotEnumerable(arrObj, "0");
verifyConfigurable(arrObj, "0");