function asyncGeneratorStep(gen, resolve, reject, _next, _throw, key, arg) { try { var info = gen[key](arg); var value = info.value; } catch (error) { reject(error); return; } if (info.done) { resolve(value); } else { Promise.resolve(value).then(_next, _throw); } }

function _asyncToGenerator(fn) { return function () { var self = this, args = arguments; return new Promise(function (resolve, reject) { var gen = fn.apply(self, args); function _next(value) { asyncGeneratorStep(gen, resolve, reject, _next, _throw, "next", value); } function _throw(err) { asyncGeneratorStep(gen, resolve, reject, _next, _throw, "throw", err); } _next(undefined); }); }; }

function _defineProperty(obj, key, value) { if (key in obj) { Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true }); } else { obj[key] = value; } return obj; }

function _asyncIterator(iterable) { var method, async, sync, retry = 2; for ("undefined" != typeof Symbol && (async = Symbol.asyncIterator, sync = Symbol.iterator); retry--;) { if (async && null != (method = iterable[async])) return method.call(iterable); if (sync && null != (method = iterable[sync])) return new AsyncFromSyncIterator(method.call(iterable)); async = "@@asyncIterator", sync = "@@iterator"; } throw new TypeError("Object is not async iterable"); }

function AsyncFromSyncIterator(s) { function AsyncFromSyncIteratorContinuation(r) { if (Object(r) !== r) return Promise.reject(new TypeError(r + " is not an object.")); var done = r.done; return Promise.resolve(r.value).then(function (value) { return { value: value, done: done }; }); } return AsyncFromSyncIterator = function AsyncFromSyncIterator(s) { this.s = s, this.n = s.next; }, AsyncFromSyncIterator.prototype = { s: null, n: null, next: function next() { return AsyncFromSyncIteratorContinuation(this.n.apply(this.s, arguments)); }, "return": function _return(value) { var ret = this.s["return"]; return void 0 === ret ? Promise.resolve({ value: value, done: !0 }) : AsyncFromSyncIteratorContinuation(ret.apply(this.s, arguments)); }, "throw": function _throw(value) { var thr = this.s["return"]; return void 0 === thr ? Promise.reject(value) : AsyncFromSyncIteratorContinuation(thr.apply(this.s, arguments)); } }, new AsyncFromSyncIterator(s); }

// Copyright 2019 Google, LLC.  All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.

/*---
esid: prod-OptionalExpression
description: >
  optional chain RHS of for await statement
info: |
  IterationStatement
    for await (LeftHandSideExpression of AssignmentExpression) Statement
features: [optional-chaining]
flags: [async]
---*/
var obj = {
  iterable: _defineProperty({}, Symbol.asyncIterator, function () {
    return {
      i: 0,
      next: function next() {
        if (this.i < 3) {
          return Promise.resolve({
            value: this.i++,
            done: false
          });
        }

        return Promise.resolve({
          done: true
        });
      }
    };
  })
};

function checkAssertions() {
  return _checkAssertions.apply(this, arguments);
}

function _checkAssertions() {
  _checkAssertions = _asyncToGenerator( /*#__PURE__*/regeneratorRuntime.mark(function _callee() {
    var count, _iteratorAbruptCompletion, _didIteratorError, _iteratorError, _iterator, _step, num;

    return regeneratorRuntime.wrap(function _callee$(_context) {
      while (1) {
        switch (_context.prev = _context.next) {
          case 0:
            count = 0;
            _iteratorAbruptCompletion = false;
            _didIteratorError = false;
            _context.prev = 3;
            _iterator = _asyncIterator(obj === null || obj === void 0 ? void 0 : obj.iterable);

          case 5:
            _context.next = 7;
            return _iterator.next();

          case 7:
            if (!(_iteratorAbruptCompletion = !(_step = _context.sent).done)) {
              _context.next = 13;
              break;
            }

            num = _step.value;
            count += num;

          case 10:
            _iteratorAbruptCompletion = false;
            _context.next = 5;
            break;

          case 13:
            _context.next = 19;
            break;

          case 15:
            _context.prev = 15;
            _context.t0 = _context["catch"](3);
            _didIteratorError = true;
            _iteratorError = _context.t0;

          case 19:
            _context.prev = 19;
            _context.prev = 20;

            if (!(_iteratorAbruptCompletion && _iterator["return"] != null)) {
              _context.next = 24;
              break;
            }

            _context.next = 24;
            return _iterator["return"]();

          case 24:
            _context.prev = 24;

            if (!_didIteratorError) {
              _context.next = 27;
              break;
            }

            throw _iteratorError;

          case 27:
            return _context.finish(24);

          case 28:
            return _context.finish(19);

          case 29:
            assert.sameValue(3, count);

          case 30:
          case "end":
            return _context.stop();
        }
      }
    }, _callee, null, [[3, 15, 19, 29], [20,, 24, 28]]);
  }));
  return _checkAssertions.apply(this, arguments);
}

checkAssertions().then($DONE, $DONE);