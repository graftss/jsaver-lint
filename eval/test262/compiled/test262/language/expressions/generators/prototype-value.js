// Copyright (C) 2015 the V8 project authors. All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.

/*---
description: >
    Generator instances directly inherit properties from the object that is the
    value of the prototype property of the Generator function that created the
    instance.
es6id: 25.3
features: [generators]
---*/
var g = /*#__PURE__*/regeneratorRuntime.mark(function g() {
  return regeneratorRuntime.wrap(function g$(_context) {
    while (1) {
      switch (_context.prev = _context.next) {
        case 0:
        case "end":
          return _context.stop();
      }
    }
  }, g);
});
assert.sameValue(Object.getPrototypeOf(g()), g.prototype, 'Instance created via function invocation');