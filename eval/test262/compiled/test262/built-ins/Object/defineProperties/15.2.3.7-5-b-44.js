// Copyright (c) 2012 Ecma International.  All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.

/*---
es5id: 15.2.3.7-5-b-44
description: >
    Object.defineProperties - value of 'enumerable' property of
    'descObj' is a Function object (8.10.5 step 3.b)
---*/
var obj = {};
var accessed = false;
Object.defineProperties(obj, {
  prop: {
    enumerable: function enumerable() {}
  }
});

for (var property in obj) {
  if (property === "prop") {
    accessed = true;
  }
}

assert(accessed, 'accessed !== true');