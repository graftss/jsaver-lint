function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } }

function _createClass(Constructor, protoProps, staticProps) { if (protoProps) _defineProperties(Constructor.prototype, protoProps); if (staticProps) _defineProperties(Constructor, staticProps); Object.defineProperty(Constructor, "prototype", { writable: false }); return Constructor; }

// Copyright (C) 2013 the V8 project authors. All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.

/*---
  description: >
      `yield` is a valid expression within generator function bodies.
  features: [generators]
  es6id: 14.4
---*/
var iter, result;

var A = /*#__PURE__*/function () {
  function A() {
    _classCallCheck(this, A);
  }

  _createClass(A, [{
    key: "g1",
    value: /*#__PURE__*/regeneratorRuntime.mark(function g1() {
      return regeneratorRuntime.wrap(function g1$(_context) {
        while (1) {
          switch (_context.prev = _context.next) {
            case 0:
              _context.next = 2;
              return;

            case 2:
            case "end":
              return _context.stop();
          }
        }
      }, g1);
    })
  }, {
    key: "g2",
    value: /*#__PURE__*/regeneratorRuntime.mark(function g2() {
      return regeneratorRuntime.wrap(function g2$(_context2) {
        while (1) {
          switch (_context2.prev = _context2.next) {
            case 0:
              _context2.next = 2;
              return;

            case 2:
              _context2.t0 = _context2.sent;
              [_context2.t0];

            case 4:
            case "end":
              return _context2.stop();
          }
        }
      }, g2);
    })
  }, {
    key: "g3",
    value: /*#__PURE__*/regeneratorRuntime.mark(function g3() {
      return regeneratorRuntime.wrap(function g3$(_context3) {
        while (1) {
          switch (_context3.prev = _context3.next) {
            case 0:
              _context3.next = 2;
              return;

            case 2:
            case "end":
              return _context3.stop();
          }
        }
      }, g3);
    })
  }, {
    key: "g4",
    value: /*#__PURE__*/regeneratorRuntime.mark(function g4() {
      return regeneratorRuntime.wrap(function g4$(_context4) {
        while (1) {
          switch (_context4.prev = _context4.next) {
            case 0:
              _context4.next = 2;
              return;

            case 2:
              _context4.next = 4;
              return;

            case 4:
            case "end":
              return _context4.stop();
          }
        }
      }, g4);
    })
  }, {
    key: "g5",
    value: /*#__PURE__*/regeneratorRuntime.mark(function g5() {
      return regeneratorRuntime.wrap(function g5$(_context5) {
        while (1) {
          switch (_context5.prev = _context5.next) {
            case 0:
              _context5.next = 2;
              return;

            case 2:
              if (!_context5.sent) {
                _context5.next = 7;
                break;
              }

              _context5.next = 5;
              return;

            case 5:
              _context5.next = 9;
              break;

            case 7:
              _context5.next = 9;
              return;

            case 9:
            case "end":
              return _context5.stop();
          }
        }
      }, g5);
    })
  }]);

  return A;
}();

iter = A.prototype.g1();
result = iter.next();
assert.sameValue(result.value, undefined, 'Within grouping operator: result `value`');
assert.sameValue(result.done, false, 'Within grouping operator: result `done` flag');
result = iter.next();
assert.sameValue(result.value, undefined, 'Following grouping operator: result `value`');
assert.sameValue(result.done, true, 'Following grouping operator: result `done` flag');
iter = A.prototype.g2();
result = iter.next();
assert.sameValue(result.value, undefined, 'Within array literal: result `value`');
assert.sameValue(result.done, false, 'Within array literal: result `done` flag');
result = iter.next();
assert.sameValue(result.value, undefined, 'Following array literal: result `value`');
assert.sameValue(result.done, true, 'Following array literal: result `done` flag');
iter = A.prototype.g3();
result = iter.next();
assert.sameValue(result.value, undefined, 'Within object literal: result `value`');
assert.sameValue(result.done, false, 'Within object literal: result `done` flag');
result = iter.next();
assert.sameValue(result.value, undefined, 'Following object literal: result `value`');
assert.sameValue(result.done, true, 'Following object literal: result `done` flag');
iter = A.prototype.g4();
result = iter.next();
assert.sameValue(result.value, undefined, 'First expression in comma expression: result `value`');
assert.sameValue(result.done, false, 'First expression in comma expression: result `done` flag');
result = iter.next();
assert.sameValue(result.value, undefined, 'Second expression in comma expression: result `value`');
assert.sameValue(result.done, false, 'Second expression in comma expression: result `done` flag');
result = iter.next();
assert.sameValue(result.value, undefined, 'Following comma expression: result `value`');
assert.sameValue(result.done, true, 'Following comma expression: result `done` flag');
iter = A.prototype.g5();
result = iter.next();
assert.sameValue(result.value, undefined, 'Conditional expression in conditional operator: result `value`');
assert.sameValue(result.done, false, 'Conditional expression in conditional operator: result `done` flag');
result = iter.next();
assert.sameValue(result.value, undefined, 'Branch in conditional operator: result `value`');
assert.sameValue(result.done, false, 'Branch in conditional operator: result `done` flag');
result = iter.next();
assert.sameValue(result.value, undefined, 'Following conditional operator: result `value`');
assert.sameValue(result.done, true, 'Following conditional operator: result `done` flag');