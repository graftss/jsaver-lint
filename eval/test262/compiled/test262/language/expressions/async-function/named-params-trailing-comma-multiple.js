var _this = this;

function _newArrowCheck(innerThis, boundThis) { if (innerThis !== boundThis) { throw new TypeError("Cannot instantiate an arrow function"); } }

function asyncGeneratorStep(gen, resolve, reject, _next, _throw, key, arg) { try { var info = gen[key](arg); var value = info.value; } catch (error) { reject(error); return; } if (info.done) { resolve(value); } else { Promise.resolve(value).then(_next, _throw); } }

function _asyncToGenerator(fn) { return function () { var self = this, args = arguments; return new Promise(function (resolve, reject) { var gen = fn.apply(self, args); function _next(value) { asyncGeneratorStep(gen, resolve, reject, _next, _throw, "next", value); } function _throw(err) { asyncGeneratorStep(gen, resolve, reject, _next, _throw, "throw", err); } _next(undefined); }); }; }

// This file was procedurally generated from the following sources:
// - src/function-forms/params-trailing-comma-multiple.case
// - src/function-forms/default/async-func-expr-named.template

/*---
description: A trailing comma should not increase the respective length, using multiple parameters (async function named expression)
esid: sec-async-function-definitions
features: [async-functions]
flags: [generated, async]
info: |
    14.6 Async Function Definitions

    AsyncFunctionExpression :
      async function BindingIdentifier ( FormalParameters ) { AsyncFunctionBody }


    Trailing comma in the parameters list

    14.1 Function Definitions

    FormalParameters[Yield, Await] : FormalParameterList[?Yield, ?Await] ,
---*/
var callCount = 0; // Stores a reference `ref` for case evaluation

var ref;

ref = /*#__PURE__*/function () {
  var _ref = _asyncToGenerator( /*#__PURE__*/regeneratorRuntime.mark(function _callee(a, b) {
    return regeneratorRuntime.wrap(function _callee$(_context) {
      while (1) {
        switch (_context.prev = _context.next) {
          case 0:
            assert.sameValue(a, 42);
            assert.sameValue(b, 39);
            callCount = callCount + 1;

          case 3:
          case "end":
            return _context.stop();
        }
      }
    }, _callee);
  }));

  function ref(_x, _x2) {
    return _ref.apply(this, arguments);
  }

  return ref;
}();

ref(42, 39, 1).then(function () {
  _newArrowCheck(this, _this);

  assert.sameValue(callCount, 1, 'function invoked exactly once');
}.bind(this)).then($DONE, $DONE);
assert.sameValue(ref.length, 2, 'length is properly set');