// Copyright (C) 2016 the V8 project authors. All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.

/*---
es6id: 14.1.6
description: >
  Default parameters' effect on function length
info: |
  Function length is counted by the non initialized parameters in the left.

  9.2.4 FunctionInitialize (F, kind, ParameterList, Body, Scope)

  [...]
  2. Let len be the ExpectedArgumentCount of ParameterList.
  3. Perform ! DefinePropertyOrThrow(F, "length", PropertyDescriptor{[[Value]]:
     len, [[Writable]]: false, [[Enumerable]]: false, [[Configurable]]: true}).
  [...]

  FormalsList : FormalParameter

    1. If HasInitializer of FormalParameter is true return 0
    2. Return 1.

  FormalsList : FormalsList , FormalParameter

    1. Let count be the ExpectedArgumentCount of FormalsList.
    2. If HasInitializer of FormalsList is true or HasInitializer of
    FormalParameter is true, return count.
    3. Return count+1.
features: [generators, default-parameters]
includes: [propertyHelper.js]
---*/
var f1 = {
  m: /*#__PURE__*/regeneratorRuntime.mark(function m() {
    var x,
        _args = arguments;
    return regeneratorRuntime.wrap(function m$(_context) {
      while (1) {
        switch (_context.prev = _context.next) {
          case 0:
            x = _args.length > 0 && _args[0] !== undefined ? _args[0] : 42;

          case 1:
          case "end":
            return _context.stop();
        }
      }
    }, m);
  })
}.m;
assert.sameValue(f1.length, 0, 'FormalsList: x = 42');
verifyNotEnumerable(f1, 'length');
verifyNotWritable(f1, 'length');
verifyConfigurable(f1, 'length');
var f2 = {
  m: /*#__PURE__*/regeneratorRuntime.mark(function m() {
    var x,
        y,
        _args2 = arguments;
    return regeneratorRuntime.wrap(function m$(_context2) {
      while (1) {
        switch (_context2.prev = _context2.next) {
          case 0:
            x = _args2.length > 0 && _args2[0] !== undefined ? _args2[0] : 42;
            y = _args2.length > 1 ? _args2[1] : undefined;

          case 2:
          case "end":
            return _context2.stop();
        }
      }
    }, m);
  })
}.m;
assert.sameValue(f2.length, 0, 'FormalsList: x = 42, y');
verifyNotEnumerable(f2, 'length');
verifyNotWritable(f2, 'length');
verifyConfigurable(f2, 'length');
var f3 = {
  m: /*#__PURE__*/regeneratorRuntime.mark(function m(x) {
    var y,
        _args3 = arguments;
    return regeneratorRuntime.wrap(function m$(_context3) {
      while (1) {
        switch (_context3.prev = _context3.next) {
          case 0:
            y = _args3.length > 1 && _args3[1] !== undefined ? _args3[1] : 42;

          case 1:
          case "end":
            return _context3.stop();
        }
      }
    }, m);
  })
}.m;
assert.sameValue(f3.length, 1, 'FormalsList: x, y = 42');
verifyNotEnumerable(f3, 'length');
verifyNotWritable(f3, 'length');
verifyConfigurable(f3, 'length');
var f4 = {
  m: /*#__PURE__*/regeneratorRuntime.mark(function m(x) {
    var y,
        z,
        _args4 = arguments;
    return regeneratorRuntime.wrap(function m$(_context4) {
      while (1) {
        switch (_context4.prev = _context4.next) {
          case 0:
            y = _args4.length > 1 && _args4[1] !== undefined ? _args4[1] : 42;
            z = _args4.length > 2 ? _args4[2] : undefined;

          case 2:
          case "end":
            return _context4.stop();
        }
      }
    }, m);
  })
}.m;
assert.sameValue(f4.length, 1, 'FormalsList: x, y = 42, z');
verifyNotEnumerable(f4, 'length');
verifyNotWritable(f4, 'length');
verifyConfigurable(f4, 'length');