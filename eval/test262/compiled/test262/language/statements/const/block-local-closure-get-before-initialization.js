// Copyright (C) 2011 the V8 project authors. All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.

/*---
es6id: 13.1
description: >
    const: block local closure [[Get]] before initialization.
    (TDZ, Temporal Dead Zone)
---*/
{
  var f = function f() {
    return x + 1;
  };

  assert["throws"](ReferenceError, function () {
    f();
  });
  var x = 1;
}