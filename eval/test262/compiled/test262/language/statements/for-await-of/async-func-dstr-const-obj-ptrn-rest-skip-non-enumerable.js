var _this = this;

function _extends() { _extends = Object.assign || function (target) { for (var i = 1; i < arguments.length; i++) { var source = arguments[i]; for (var key in source) { if (Object.prototype.hasOwnProperty.call(source, key)) { target[key] = source[key]; } } } return target; }; return _extends.apply(this, arguments); }

function _newArrowCheck(innerThis, boundThis) { if (innerThis !== boundThis) { throw new TypeError("Cannot instantiate an arrow function"); } }

function asyncGeneratorStep(gen, resolve, reject, _next, _throw, key, arg) { try { var info = gen[key](arg); var value = info.value; } catch (error) { reject(error); return; } if (info.done) { resolve(value); } else { Promise.resolve(value).then(_next, _throw); } }

function _asyncToGenerator(fn) { return function () { var self = this, args = arguments; return new Promise(function (resolve, reject) { var gen = fn.apply(self, args); function _next(value) { asyncGeneratorStep(gen, resolve, reject, _next, _throw, "next", value); } function _throw(err) { asyncGeneratorStep(gen, resolve, reject, _next, _throw, "throw", err); } _next(undefined); }); }; }

function _asyncIterator(iterable) { var method, async, sync, retry = 2; for ("undefined" != typeof Symbol && (async = Symbol.asyncIterator, sync = Symbol.iterator); retry--;) { if (async && null != (method = iterable[async])) return method.call(iterable); if (sync && null != (method = iterable[sync])) return new AsyncFromSyncIterator(method.call(iterable)); async = "@@asyncIterator", sync = "@@iterator"; } throw new TypeError("Object is not async iterable"); }

function AsyncFromSyncIterator(s) { function AsyncFromSyncIteratorContinuation(r) { if (Object(r) !== r) return Promise.reject(new TypeError(r + " is not an object.")); var done = r.done; return Promise.resolve(r.value).then(function (value) { return { value: value, done: done }; }); } return AsyncFromSyncIterator = function AsyncFromSyncIterator(s) { this.s = s, this.n = s.next; }, AsyncFromSyncIterator.prototype = { s: null, n: null, next: function next() { return AsyncFromSyncIteratorContinuation(this.n.apply(this.s, arguments)); }, "return": function _return(value) { var ret = this.s["return"]; return void 0 === ret ? Promise.resolve({ value: value, done: !0 }) : AsyncFromSyncIteratorContinuation(ret.apply(this.s, arguments)); }, "throw": function _throw(value) { var thr = this.s["return"]; return void 0 === thr ? Promise.reject(value) : AsyncFromSyncIteratorContinuation(thr.apply(this.s, arguments)); } }, new AsyncFromSyncIterator(s); }

// This file was procedurally generated from the following sources:
// - src/dstr-binding-for-await/obj-ptrn-rest-skip-non-enumerable.case
// - src/dstr-binding-for-await/default/for-await-of-async-func-const.template

/*---
description: Rest object doesn't contain non-enumerable properties (for-await-of statement)
esid: sec-for-in-and-for-of-statements-runtime-semantics-labelledevaluation
features: [object-rest, destructuring-binding, async-iteration]
flags: [generated, async]
includes: [propertyHelper.js]
info: |
    IterationStatement :
        for await ( ForDeclaration of AssignmentExpression ) Statement

    [...]
    2. Return ? ForIn/OfBodyEvaluation(ForDeclaration, Statement, keyResult,
        lexicalBinding, labelSet, async).

    13.7.5.13 Runtime Semantics: ForIn/OfBodyEvaluation

    [...]
    4. Let destructuring be IsDestructuring of lhs.
    [...]
    6. Repeat
       [...]
       j. If destructuring is false, then
          [...]
       k. Else
          i. If lhsKind is assignment, then
             [...]
          ii. Else if lhsKind is varBinding, then
              [...]
          iii. Else,
               1. Assert: lhsKind is lexicalBinding.
               2. Assert: lhs is a ForDeclaration.
               3. Let status be the result of performing BindingInitialization
                  for lhs passing nextValue and iterationEnv as arguments.
          [...]
---*/
var o = {
  a: 3,
  b: 4
};
Object.defineProperty(o, "x", {
  value: 4,
  enumerable: false
});
var iterCount = 0;

function fn() {
  return _fn.apply(this, arguments);
}

function _fn() {
  _fn = _asyncToGenerator( /*#__PURE__*/regeneratorRuntime.mark(function _callee() {
    var _iteratorAbruptCompletion, _didIteratorError, _iteratorError, _iterator, _step, rest;

    return regeneratorRuntime.wrap(function _callee$(_context) {
      while (1) {
        switch (_context.prev = _context.next) {
          case 0:
            _iteratorAbruptCompletion = false;
            _didIteratorError = false;
            _context.prev = 2;
            _iterator = _asyncIterator([o]);

          case 4:
            _context.next = 6;
            return _iterator.next();

          case 6:
            if (!(_iteratorAbruptCompletion = !(_step = _context.sent).done)) {
              _context.next = 17;
              break;
            }

            rest = _extends({}, _step.value);
            assert.sameValue(rest.a, 3);
            assert.sameValue(rest.b, 4);
            assert.sameValue(rest.x, undefined);
            verifyProperty(rest, "a", {
              enumerable: true,
              writable: true,
              configurable: true,
              value: 3
            });
            verifyProperty(rest, "b", {
              enumerable: true,
              writable: true,
              configurable: true,
              value: 4
            });
            iterCount += 1;

          case 14:
            _iteratorAbruptCompletion = false;
            _context.next = 4;
            break;

          case 17:
            _context.next = 23;
            break;

          case 19:
            _context.prev = 19;
            _context.t0 = _context["catch"](2);
            _didIteratorError = true;
            _iteratorError = _context.t0;

          case 23:
            _context.prev = 23;
            _context.prev = 24;

            if (!(_iteratorAbruptCompletion && _iterator["return"] != null)) {
              _context.next = 28;
              break;
            }

            _context.next = 28;
            return _iterator["return"]();

          case 28:
            _context.prev = 28;

            if (!_didIteratorError) {
              _context.next = 31;
              break;
            }

            throw _iteratorError;

          case 31:
            return _context.finish(28);

          case 32:
            return _context.finish(23);

          case 33:
          case "end":
            return _context.stop();
        }
      }
    }, _callee, null, [[2, 19, 23, 33], [24,, 28, 32]]);
  }));
  return _fn.apply(this, arguments);
}

fn().then(function () {
  _newArrowCheck(this, _this);

  return assert.sameValue(iterCount, 1, 'iteration occurred as expected');
}.bind(this), $DONE).then($DONE, $DONE);