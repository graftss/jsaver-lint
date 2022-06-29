function _typeof(obj) { "@babel/helpers - typeof"; return _typeof = "function" == typeof Symbol && "symbol" == typeof Symbol.iterator ? function (obj) { return typeof obj; } : function (obj) { return obj && "function" == typeof Symbol && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : typeof obj; }, _typeof(obj); }

// Copyright (C) 2015 the V8 project authors. All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.

/*---
es6id: 23.2.3.9
description: >
    get Set.prototype.size

    17 ECMAScript Standard Built-in Objects

includes: [propertyHelper.js]
---*/
var descriptor = Object.getOwnPropertyDescriptor(Set.prototype, "size");
assert.sameValue(_typeof(descriptor.get), "function", "`typeof descriptor.get` is `'function'`");
assert.sameValue(_typeof(descriptor.set), "undefined", "`typeof descriptor.set` is `\"undefined\"`");
verifyNotEnumerable(Set.prototype, "size");
verifyConfigurable(Set.prototype, "size");