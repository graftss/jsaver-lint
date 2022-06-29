function _typeof(obj) { "@babel/helpers - typeof"; return _typeof = "function" == typeof Symbol && "symbol" == typeof Symbol.iterator ? function (obj) { return typeof obj; } : function (obj) { return obj && "function" == typeof Symbol && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : typeof obj; }, _typeof(obj); }

// Copyright (C) 2016 the V8 project authors. All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.

/*---
esid: sec-string.prototype.split
description: The "this" value must be object-coercible
info: |
  1. Let O be ? RequireObjectCoercible(this value).
---*/
var split = String.prototype.split;
assert.sameValue(_typeof(split), 'function');
assert["throws"](TypeError, function () {
  split.call(undefined, '');
}, 'undefined');
assert["throws"](TypeError, function () {
  split.call(null, '');
}, 'null');