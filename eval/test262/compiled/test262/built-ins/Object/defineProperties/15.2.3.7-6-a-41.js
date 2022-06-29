// Copyright (c) 2012 Ecma International.  All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.

/*---
es5id: 15.2.3.7-6-a-41
description: >
    Object.defineProperties - type of desc.value is different from
    type of P.value (8.12.9 step 6)
includes: [propertyHelper.js]
---*/
var obj = {};
obj.foo = 101; // default value of attributes: writable: true, configurable: true, enumerable: true

Object.defineProperties(obj, {
  foo: {
    value: "102"
  }
});
verifyEqualTo(obj, "foo", "102");
verifyWritable(obj, "foo");
verifyEnumerable(obj, "foo");
verifyConfigurable(obj, "foo");