// This file was procedurally generated from the following sources:
// - src/dstr-binding/obj-ptrn-id-init-skipped.case
// - src/dstr-binding/default/gen-func-expr.template

/*---
description: Destructuring initializer is not evaluated when value is not `undefined` (generator function expression)
esid: sec-generator-function-definitions-runtime-semantics-evaluation
features: [generators, destructuring-binding]
flags: [generated]
info: |
    GeneratorExpression : function * ( FormalParameters ) { GeneratorBody }

        [...]
        3. Let closure be GeneratorFunctionCreate(Normal, FormalParameters,
           GeneratorBody, scope, strict).
        [...]

    9.2.1 [[Call]] ( thisArgument, argumentsList)

    [...]
    7. Let result be OrdinaryCallEvaluateBody(F, argumentsList).
    [...]

    9.2.1.3 OrdinaryCallEvaluateBody ( F, argumentsList )

    1. Let status be FunctionDeclarationInstantiation(F, argumentsList).
    [...]

    9.2.12 FunctionDeclarationInstantiation(func, argumentsList)

    [...]
    23. Let iteratorRecord be Record {[[iterator]]:
        CreateListIterator(argumentsList), [[done]]: false}.
    24. If hasDuplicates is true, then
        [...]
    25. Else,
        b. Let formalStatus be IteratorBindingInitialization for formals with
           iteratorRecord and env as arguments.
    [...]

    13.3.3.7 Runtime Semantics: KeyedBindingInitialization

    SingleNameBinding : BindingIdentifier Initializeropt

    [...]
    6. If Initializer is present and v is undefined, then
       [...]
    [...]
---*/
var initCount = 0;

function counter() {
  initCount += 1;
}

var callCount = 0;
var f;
f = /*#__PURE__*/regeneratorRuntime.mark(function f(_ref) {
  var _ref$w, w, _ref$x, x, _ref$y, y, _ref$z, z;

  return regeneratorRuntime.wrap(function f$(_context) {
    while (1) {
      switch (_context.prev = _context.next) {
        case 0:
          _ref$w = _ref.w, w = _ref$w === void 0 ? counter() : _ref$w, _ref$x = _ref.x, x = _ref$x === void 0 ? counter() : _ref$x, _ref$y = _ref.y, y = _ref$y === void 0 ? counter() : _ref$y, _ref$z = _ref.z, z = _ref$z === void 0 ? counter() : _ref$z;
          assert.sameValue(w, null);
          assert.sameValue(x, 0);
          assert.sameValue(y, false);
          assert.sameValue(z, '');
          assert.sameValue(initCount, 0);
          callCount = callCount + 1;

        case 7:
        case "end":
          return _context.stop();
      }
    }
  }, f);
});
f({
  w: null,
  x: 0,
  y: false,
  z: ''
}).next();
assert.sameValue(callCount, 1, 'generator function invoked exactly once');