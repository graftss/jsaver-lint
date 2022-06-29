var _marked = /*#__PURE__*/regeneratorRuntime.mark(g);

// Copyright (C) 2013 the V8 project authors. All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.

/*---
description: >
    The GeneratorPrototype intrinsic should define a `constructor` property
    that is non-enumerable, non-writable, and configurable.
includes: [propertyHelper.js]
es6id: 25.3.1
features: [generators]
---*/
function g() {
  return regeneratorRuntime.wrap(function g$(_context) {
    while (1) {
      switch (_context.prev = _context.next) {
        case 0:
        case "end":
          return _context.stop();
      }
    }
  }, _marked);
}

var Generator = Object.getPrototypeOf(g);
var GeneratorPrototype = Generator.prototype;
assert.sameValue(GeneratorPrototype.constructor, Generator);
verifyNotEnumerable(GeneratorPrototype, 'constructor');
verifyNotWritable(GeneratorPrototype, 'constructor');
verifyConfigurable(GeneratorPrototype, 'constructor');