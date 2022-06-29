var _this = this;

function _newArrowCheck(innerThis, boundThis) { if (innerThis !== boundThis) { throw new TypeError("Cannot instantiate an arrow function"); } }

// Copyright (C) 2015 the V8 project authors. All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.

/*---
es6id: 14.2
description: >
    No need for parentheses even for lower-precedence expression body
---*/
var square = function square(x) {
  _newArrowCheck(this, _this);

  return x * x;
}.bind(this);

assert.sameValue(square(3), 9);