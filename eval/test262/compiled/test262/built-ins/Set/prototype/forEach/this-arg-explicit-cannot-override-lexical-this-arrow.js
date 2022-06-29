var _this = this;

function _newArrowCheck(innerThis, boundThis) { if (innerThis !== boundThis) { throw new TypeError("Cannot instantiate an arrow function"); } }

// Copyright (C) 2015 the V8 project authors. All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.

/*---
es6id: 23.2.3.6
description: >
    Set.prototype.forEach ( callbackfn [ , thisArg ] )

    ...
    5. If thisArg was supplied, let T be thisArg; else let T be undefined.
    ...

    An arrow function will ignore an explicit thisArg

features: [arrow-function]
---*/
var s = new Set([1]);
var usurper = {};
var counter = 0;
s.forEach(function (_) {
  _newArrowCheck(this, _this);

  assert.notSameValue(this, usurper, "`this` is not `usurper`");
  counter++;
}.bind(this), usurper);
assert.sameValue(counter, 1, "`forEach` is not a no-op");