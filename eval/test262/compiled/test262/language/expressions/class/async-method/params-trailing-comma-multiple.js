var _this = this;

function _newArrowCheck(innerThis, boundThis) { if (innerThis !== boundThis) { throw new TypeError("Cannot instantiate an arrow function"); } }

function asyncGeneratorStep(gen, resolve, reject, _next, _throw, key, arg) { try { var info = gen[key](arg); var value = info.value; } catch (error) { reject(error); return; } if (info.done) { resolve(value); } else { Promise.resolve(value).then(_next, _throw); } }

function _asyncToGenerator(fn) { return function () { var self = this, args = arguments; return new Promise(function (resolve, reject) { var gen = fn.apply(self, args); function _next(value) { asyncGeneratorStep(gen, resolve, reject, _next, _throw, "next", value); } function _throw(err) { asyncGeneratorStep(gen, resolve, reject, _next, _throw, "throw", err); } _next(undefined); }); }; }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } }

function _createClass(Constructor, protoProps, staticProps) { if (protoProps) _defineProperties(Constructor.prototype, protoProps); if (staticProps) _defineProperties(Constructor, staticProps); Object.defineProperty(Constructor, "prototype", { writable: false }); return Constructor; }

// This file was procedurally generated from the following sources:
// - src/function-forms/params-trailing-comma-multiple.case
// - src/function-forms/default/cls-expr-async-meth.template

/*---
description: A trailing comma should not increase the respective length, using multiple parameters (class expression async method)
esid: sec-class-definitions-runtime-semantics-evaluation
features: [async-functions]
flags: [generated, async]
info: |
    ClassExpression : class BindingIdentifieropt ClassTail

    1. If BindingIdentifieropt is not present, let className be undefined.
    2. Else, let className be StringValue of BindingIdentifier.
    3. Let value be the result of ClassDefinitionEvaluation of ClassTail
        with argument className.
    [...]

    14.5.14 Runtime Semantics: ClassDefinitionEvaluation

    21. For each ClassElement m in order from methods
      a. If IsStatic of m is false, then
        i. Let status be the result of performing
          PropertyDefinitionEvaluation for m with arguments proto and
          false.
      [...]

    Runtime Semantics: PropertyDefinitionEvaluation

    AsyncMethod : async PropertyName ( UniqueFormalParameters ) { AsyncFunctionBody }

    1. Let propKey be the result of evaluating PropertyName.
    2. ReturnIfAbrupt(propKey).
    3. If the function code for this AsyncMethod is strict mode code, let strict be true. Otherwise
       let strict be false.
    4. Let scope be the LexicalEnvironment of the running execution context.
    5. Let closure be ! AsyncFunctionCreate(Method, UniqueFormalParameters, AsyncFunctionBody,
       scope, strict).
    [...]


    Trailing comma in the parameters list

    14.1 Function Definitions

    FormalParameters[Yield, Await] : FormalParameterList[?Yield, ?Await] ,
---*/
var callCount = 0;

var C = /*#__PURE__*/function () {
  function C() {
    _classCallCheck(this, C);
  }

  _createClass(C, [{
    key: "method",
    value: function () {
      var _method = _asyncToGenerator( /*#__PURE__*/regeneratorRuntime.mark(function _callee(a, b) {
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

      function method(_x, _x2) {
        return _method.apply(this, arguments);
      }

      return method;
    }()
  }]);

  return C;
}(); // Stores a reference `ref` for case evaluation


var ref = C.prototype.method;
ref(42, 39, 1).then(function () {
  _newArrowCheck(this, _this);

  assert.sameValue(callCount, 1, 'method invoked exactly once');
}.bind(this)).then($DONE, $DONE);
assert.sameValue(ref.length, 2, 'length is properly set');