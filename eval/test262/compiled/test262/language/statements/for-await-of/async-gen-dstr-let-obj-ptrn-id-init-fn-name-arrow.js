var _this2 = this;

function _newArrowCheck(innerThis, boundThis) { if (innerThis !== boundThis) { throw new TypeError("Cannot instantiate an arrow function"); } }

function _awaitAsyncGenerator(value) { return new _AwaitValue(value); }

function _wrapAsyncGenerator(fn) { return function () { return new _AsyncGenerator(fn.apply(this, arguments)); }; }

function _AsyncGenerator(gen) { var front, back; function send(key, arg) { return new Promise(function (resolve, reject) { var request = { key: key, arg: arg, resolve: resolve, reject: reject, next: null }; if (back) { back = back.next = request; } else { front = back = request; resume(key, arg); } }); } function resume(key, arg) { try { var result = gen[key](arg); var value = result.value; var wrappedAwait = value instanceof _AwaitValue; Promise.resolve(wrappedAwait ? value.wrapped : value).then(function (arg) { if (wrappedAwait) { resume(key === "return" ? "return" : "next", arg); return; } settle(result.done ? "return" : "normal", arg); }, function (err) { resume("throw", err); }); } catch (err) { settle("throw", err); } } function settle(type, value) { switch (type) { case "return": front.resolve({ value: value, done: true }); break; case "throw": front.reject(value); break; default: front.resolve({ value: value, done: false }); break; } front = front.next; if (front) { resume(front.key, front.arg); } else { back = null; } } this._invoke = send; if (typeof gen["return"] !== "function") { this["return"] = undefined; } }

_AsyncGenerator.prototype[typeof Symbol === "function" && Symbol.asyncIterator || "@@asyncIterator"] = function () { return this; };

_AsyncGenerator.prototype.next = function (arg) { return this._invoke("next", arg); };

_AsyncGenerator.prototype["throw"] = function (arg) { return this._invoke("throw", arg); };

_AsyncGenerator.prototype["return"] = function (arg) { return this._invoke("return", arg); };

function _AwaitValue(value) { this.wrapped = value; }

function _asyncIterator(iterable) { var method, async, sync, retry = 2; for ("undefined" != typeof Symbol && (async = Symbol.asyncIterator, sync = Symbol.iterator); retry--;) { if (async && null != (method = iterable[async])) return method.call(iterable); if (sync && null != (method = iterable[sync])) return new AsyncFromSyncIterator(method.call(iterable)); async = "@@asyncIterator", sync = "@@iterator"; } throw new TypeError("Object is not async iterable"); }

function AsyncFromSyncIterator(s) { function AsyncFromSyncIteratorContinuation(r) { if (Object(r) !== r) return Promise.reject(new TypeError(r + " is not an object.")); var done = r.done; return Promise.resolve(r.value).then(function (value) { return { value: value, done: done }; }); } return AsyncFromSyncIterator = function AsyncFromSyncIterator(s) { this.s = s, this.n = s.next; }, AsyncFromSyncIterator.prototype = { s: null, n: null, next: function next() { return AsyncFromSyncIteratorContinuation(this.n.apply(this.s, arguments)); }, "return": function _return(value) { var ret = this.s["return"]; return void 0 === ret ? Promise.resolve({ value: value, done: !0 }) : AsyncFromSyncIteratorContinuation(ret.apply(this.s, arguments)); }, "throw": function _throw(value) { var thr = this.s["return"]; return void 0 === thr ? Promise.reject(value) : AsyncFromSyncIteratorContinuation(thr.apply(this.s, arguments)); } }, new AsyncFromSyncIterator(s); }

// This file was procedurally generated from the following sources:
// - src/dstr-binding-for-await/obj-ptrn-id-init-fn-name-arrow.case
// - src/dstr-binding-for-await/default/for-await-of-async-gen-let.template

/*---
description: SingleNameBinding assigns `name` to arrow functions (for-await-of statement)
esid: sec-for-in-and-for-of-statements-runtime-semantics-labelledevaluation
features: [destructuring-binding, async-iteration]
flags: [generated, async]
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

    13.3.3.7 Runtime Semantics: KeyedBindingInitialization

    SingleNameBinding : BindingIdentifier Initializer_opt

    [...]
    6. If Initializer is present and v is undefined, then
       [...]
       d. If IsAnonymousFunctionDefinition(Initializer) is true, then
          i. Let hasNameProperty be HasOwnProperty(v, "name").
          ii. ReturnIfAbrupt(hasNameProperty).
          iii. If hasNameProperty is false, perform SetFunctionName(v,
               bindingId).
---*/
var iterCount = 0;

function fn() {
  return _fn.apply(this, arguments);
}

function _fn() {
  _fn = _wrapAsyncGenerator( /*#__PURE__*/regeneratorRuntime.mark(function _callee() {
    var _this = this;

    var _iteratorAbruptCompletion, _didIteratorError, _iteratorError, _iterator, _step, _step$value$arrow, arrow;

    return regeneratorRuntime.wrap(function _callee$(_context) {
      while (1) {
        switch (_context.prev = _context.next) {
          case 0:
            _iteratorAbruptCompletion = false;
            _didIteratorError = false;
            _context.prev = 2;
            _iterator = _asyncIterator([{}]);

          case 4:
            _context.next = 6;
            return _awaitAsyncGenerator(_iterator.next());

          case 6:
            if (!(_iteratorAbruptCompletion = !(_step = _context.sent).done)) {
              _context.next = 13;
              break;
            }

            _step$value$arrow = _step.value.arrow, arrow = _step$value$arrow === void 0 ? function () {
              _newArrowCheck(this, _this);
            }.bind(this) : _step$value$arrow;
            assert.sameValue(arrow.name, 'arrow');
            iterCount += 1;

          case 10:
            _iteratorAbruptCompletion = false;
            _context.next = 4;
            break;

          case 13:
            _context.next = 19;
            break;

          case 15:
            _context.prev = 15;
            _context.t0 = _context["catch"](2);
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
            return _awaitAsyncGenerator(_iterator["return"]());

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
          case "end":
            return _context.stop();
        }
      }
    }, _callee, this, [[2, 15, 19, 29], [20,, 24, 28]]);
  }));
  return _fn.apply(this, arguments);
}

fn().next().then(function () {
  _newArrowCheck(this, _this2);

  return assert.sameValue(iterCount, 1, 'iteration occurred as expected');
}.bind(this), $DONE).then($DONE, $DONE);