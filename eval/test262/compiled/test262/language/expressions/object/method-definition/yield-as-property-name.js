// Copyright (C) 2013 the V8 project authors. All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.

/*---
  description: >
      `yield` may be used as a literal property name in an object literal
      within generator function bodies.
  features: [generators]
  es6id: 12.1.1
---*/
var result;
var obj = {
  g: /*#__PURE__*/regeneratorRuntime.mark(function g() {
    return regeneratorRuntime.wrap(function g$(_context) {
      while (1) {
        switch (_context.prev = _context.next) {
          case 0:
            ({
              "yield": 1
            });

          case 1:
          case "end":
            return _context.stop();
        }
      }
    }, g);
  })
};
result = obj.g().next();
assert.sameValue(result.value, undefined);
assert.sameValue(result.done, true);