// Copyright (c) 2012 Ecma International.  All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.

/*---
es5id: 15.2.3.6-3-134
description: >
    Object.defineProperty - 'value' property in 'Attributes' is own
    accessor property that overrides an inherited data property
    (8.10.5 step 5.a)
---*/
var obj = {};
var proto = {
  value: "inheritedDataProperty"
};

var ConstructFun = function ConstructFun() {};

ConstructFun.prototype = proto;
var child = new ConstructFun();
Object.defineProperty(child, "value", {
  get: function get() {
    return "ownAccessorProperty";
  }
});
Object.defineProperty(obj, "property", child);
assert.sameValue(obj.property, "ownAccessorProperty", 'obj.property');