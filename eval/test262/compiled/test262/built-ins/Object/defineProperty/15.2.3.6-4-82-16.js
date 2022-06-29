// Copyright (c) 2012 Ecma International.  All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.

/*---
es5id: 15.2.3.6-4-82-16
description: >
    Object.defineProperty - Update [[Configurable]] attribute of
    'name' property to false successfully when [[Enumerable]] and
    [[Configurable]] attributes of 'name' property are true,  the
    'desc' is a generic descriptor which contains [[Enumerable]]
    attribute as true and [[Configurable]] attribute as false, 'name'
    property is an index data property (8.12.9 step 8)
includes: [propertyHelper.js]
---*/
var obj = {};
Object.defineProperty(obj, "0", {
  value: 1001,
  writable: true,
  enumerable: true,
  configurable: true
});
Object.defineProperty(obj, "0", {
  enumerable: true,
  configurable: false
});
verifyEqualTo(obj, "0", 1001);
verifyWritable(obj, "0");
verifyEnumerable(obj, "0");
verifyNotConfigurable(obj, "0");