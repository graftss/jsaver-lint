function _typeof(obj) { "@babel/helpers - typeof"; return _typeof = "function" == typeof Symbol && "symbol" == typeof Symbol.iterator ? function (obj) { return typeof obj; } : function (obj) { return obj && "function" == typeof Symbol && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : typeof obj; }, _typeof(obj); }

function _newArrowCheck(innerThis, boundThis) { if (innerThis !== boundThis) { throw new TypeError("Cannot instantiate an arrow function"); } }

// Copyright (C) 2015 the V8 project authors. All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.

/*---
es6id: 14.2.16
description: >
    Runtime Semantics: Evaluation

    1. If the function code for this ArrowFunction is strict mode code (10.2.1),
      let strict be true. Otherwise let strict be false.
    ...

flags: [onlyStrict]
---*/
assert["throws"](ReferenceError, function () {
  var _this = this;

  var af = function af(_) {
    _newArrowCheck(this, _this);

    foo = 1;
  }.bind(this);

  af();
});
assert.sameValue(typeof foo === "undefined" ? "undefined" : _typeof(foo), "undefined");