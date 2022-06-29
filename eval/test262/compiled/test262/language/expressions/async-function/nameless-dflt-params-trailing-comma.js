var _this = this;

function _newArrowCheck(innerThis, boundThis) { if (innerThis !== boundThis) { throw new TypeError("Cannot instantiate an arrow function"); } }

function asyncGeneratorStep(gen, resolve, reject, _next, _throw, key, arg) { try { var info = gen[key](arg); var value = info.value; } catch (error) { reject(error); return; } if (info.done) { resolve(value); } else { Promise.resolve(value).then(_next, _throw); } }

function _asyncToGenerator(fn) { return function () { var self = this, args = arguments; return new Promise(function (resolve, reject) { var gen = fn.apply(self, args); function _next(value) { asyncGeneratorStep(gen, resolve, reject, _next, _throw, "next", value); } function _throw(err) { asyncGeneratorStep(gen, resolve, reject, _next, _throw, "throw", err); } _next(undefined); }); }; }

// This file was procedurally generated from the following sources:
// - src/function-forms/dflt-params-trailing-comma.case
// - src/function-forms/default/async-func-expr-nameless.template

/*---
description: A trailing comma should not increase the respective length, using default parameters (async function nameless expression)
esid: sec-async-function-definitions
features: [async-functions]
flags: [generated, async]
info: |
    14.6 Async Function Definitions

    AsyncFunctionExpression :
      async function ( FormalParameters ) { AsyncFunctionBody }


    Trailing comma in the parameters list

    14.1 Function Definitions

    FormalParameters[Yield, Await] : FormalParameterList[?Yield, ?Await] ,
---*/
var callCount = 0; // Stores a reference `ref` for case evaluation

var ref;

ref = /*#__PURE__*/function () {
  var _ref = _asyncToGenerator( /*#__PURE__*/regeneratorRuntime.mark(function _callee(a) {
    var b,
        _args = arguments;
    return regeneratorRuntime.wrap(function _callee$(_context) {
      while (1) {
        switch (_context.prev = _context.next) {
          case 0:
            b = _args.length > 1 && _args[1] !== undefined ? _args[1] : 39;
            assert.sameValue(a, 42);
            assert.sameValue(b, 39);
            callCount = callCount + 1;

          case 4:
          case "end":
            return _context.stop();
        }
      }
    }, _callee);
  }));

  return function ref(_x) {
    return _ref.apply(this, arguments);
  };
}();

ref(42, undefined, 1).then(function () {
  _newArrowCheck(this, _this);

  assert.sameValue(callCount, 1, 'function invoked exactly once');
}.bind(this)).then($DONE, $DONE);
assert.sameValue(ref.length, 1, 'length is properly set');