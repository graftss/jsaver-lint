// Copyright (c) 2012 Ecma International.  All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.

/*---
esid: sec-setintegritylevel
description: >
    Object.seal - 'P' is own accessor property that overrides an
    inherited data property
includes: [propertyHelper.js]
---*/
var proto = {};
Object.defineProperty(proto, "foo", {
  value: 0,
  configurable: true
});

var ConstructFun = function ConstructFun() {};

ConstructFun.prototype = proto;
var obj = new ConstructFun();
Object.defineProperty(obj, "foo", {
  get: function get() {
    return 10;
  },
  configurable: true
});
assert(Object.isExtensible(obj));
Object.seal(obj);
verifyNotConfigurable(obj, "foo");
assert.sameValue(obj.foo, 10);