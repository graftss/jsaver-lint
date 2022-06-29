var _this = this;

function _newArrowCheck(innerThis, boundThis) { if (innerThis !== boundThis) { throw new TypeError("Cannot instantiate an arrow function"); } }

function asyncGeneratorStep(gen, resolve, reject, _next, _throw, key, arg) { try { var info = gen[key](arg); var value = info.value; } catch (error) { reject(error); return; } if (info.done) { resolve(value); } else { Promise.resolve(value).then(_next, _throw); } }

function _asyncToGenerator(fn) { return function () { var self = this, args = arguments; return new Promise(function (resolve, reject) { var gen = fn.apply(self, args); function _next(value) { asyncGeneratorStep(gen, resolve, reject, _next, _throw, "next", value); } function _throw(err) { asyncGeneratorStep(gen, resolve, reject, _next, _throw, "throw", err); } _next(undefined); }); }; }

// This file was procedurally generated from the following sources:
// - src/function-forms/dflt-params-ref-later.case
// - src/function-forms/error/async-func-decl.template

/*---
description: Referencing a parameter that occurs later in the ParameterList (async function declaration)
esid: sec-async-function-definitions
features: [default-parameters, async-functions]
flags: [generated, async]
info: |
    14.6 Async Function Definitions

    AsyncFunctionDeclaration :
      async function BindingIdentifier ( FormalParameters ) { AsyncFunctionBody }


    14.1.19 Runtime Semantics: IteratorBindingInitialization

    FormalsList : FormalsList , FormalParameter

    1. Let status be the result of performing IteratorBindingInitialization for
       FormalsList using iteratorRecord and environment as the arguments.
    2. ReturnIfAbrupt(status).
    3. Return the result of performing IteratorBindingInitialization for
       FormalParameter using iteratorRecord and environment as the arguments.

---*/
var x = 0;
var callCount = 0;

function f() {
  return _f.apply(this, arguments);
}

function _f() {
  _f = _asyncToGenerator( /*#__PURE__*/regeneratorRuntime.mark(function _callee() {
    var x,
        y,
        _args = arguments;
    return regeneratorRuntime.wrap(function _callee$(_context) {
      while (1) {
        switch (_context.prev = _context.next) {
          case 0:
            x = _args.length > 0 && _args[0] !== undefined ? _args[0] : y;
            y = _args.length > 1 ? _args[1] : undefined;
            callCount = callCount + 1;

          case 3:
          case "end":
            return _context.stop();
        }
      }
    }, _callee);
  }));
  return _f.apply(this, arguments);
}

f().then(function (_) {
  _newArrowCheck(this, _this);

  throw new Test262Error('function should not be resolved');
}.bind(this), function (error) {
  _newArrowCheck(this, _this);

  return assert.sameValue(error.constructor, ReferenceError);
}.bind(this)).then(function () {
  _newArrowCheck(this, _this);

  assert.sameValue(callCount, 0, 'function body is not evaluated');
}.bind(this), $DONE).then($DONE, $DONE);