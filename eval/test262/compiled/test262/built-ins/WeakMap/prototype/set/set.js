function _typeof(obj) { "@babel/helpers - typeof"; return _typeof = "function" == typeof Symbol && "symbol" == typeof Symbol.iterator ? function (obj) { return typeof obj; } : function (obj) { return obj && "function" == typeof Symbol && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : typeof obj; }, _typeof(obj); }

// Copyright (C) 2015 the V8 project authors. All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.

/*---
esid: sec-weakmap.prototype.set
description: WeakMap.prototype.set property descriptor
info: |
  WeakMap.prototype.set ( key, value )

  17 ECMAScript Standard Built-in Objects

includes: [propertyHelper.js]
---*/
assert.sameValue(_typeof(WeakMap.prototype.set), 'function', 'typeof WeakMap.prototype.set is "function"');
verifyNotEnumerable(WeakMap.prototype, 'set');
verifyWritable(WeakMap.prototype, 'set');
verifyConfigurable(WeakMap.prototype, 'set');