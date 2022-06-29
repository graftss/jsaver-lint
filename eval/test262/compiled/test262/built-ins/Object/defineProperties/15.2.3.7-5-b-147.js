// Copyright (c) 2012 Ecma International.  All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.

/*---
es5id: 15.2.3.7-5-b-147
description: >
    Object.defineProperties - 'writable' property of 'descObj' is own
    accessor property that overrides an inherited accessor property
    (8.10.5 step 6.a)
includes: [propertyHelper.js]
---*/
var obj = {};
var proto = {};
Object.defineProperty(proto, "writable", {
  get: function get() {
    return true;
  }
});

var Con = function Con() {};

Con.prototype = proto;
var descObj = new Con();
Object.defineProperty(descObj, "writable", {
  get: function get() {
    return false;
  }
});
Object.defineProperties(obj, {
  property: descObj
});
assert(obj.hasOwnProperty("property"));
verifyNotWritable(obj, "property");