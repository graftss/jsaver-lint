function _typeof(obj) { "@babel/helpers - typeof"; return _typeof = "function" == typeof Symbol && "symbol" == typeof Symbol.iterator ? function (obj) { return typeof obj; } : function (obj) { return obj && "function" == typeof Symbol && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : typeof obj; }, _typeof(obj); }

// Copyright (C) 2015 the V8 project authors. All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.

/*---
es6id: 19.4.2.3
description: Property descriptor
info: |
    This property has the attributes { [[Writable]]: false, [[Enumerable]]:
    false, [[Configurable]]: false }.
includes: [propertyHelper.js]
features: [Symbol.isConcatSpreadable]
---*/
assert.sameValue(_typeof(Symbol.isConcatSpreadable), 'symbol');
verifyNotEnumerable(Symbol, 'isConcatSpreadable');
verifyNotWritable(Symbol, 'isConcatSpreadable');
verifyNotConfigurable(Symbol, 'isConcatSpreadable');