// This file was procedurally generated from the following sources:
// - src/dstr-binding/obj-ptrn-id-trailing-comma.case
// - src/dstr-binding/default/gen-func-expr-dflt.template

/*---
description: Trailing comma is allowed following BindingPropertyList (generator function expression (default parameter))
esid: sec-generator-function-definitions-runtime-semantics-evaluation
features: [generators, destructuring-binding, default-parameters]
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

    13.3.3 Destructuring Binding Patterns

    ObjectBindingPattern[Yield] :
        { }
        { BindingPropertyList[?Yield] }
        { BindingPropertyList[?Yield] , }
---*/
var callCount = 0;
var f;
f = /*#__PURE__*/regeneratorRuntime.mark(function f() {
  var _ref,
      x,
      _args = arguments;

  return regeneratorRuntime.wrap(function f$(_context) {
    while (1) {
      switch (_context.prev = _context.next) {
        case 0:
          _ref = _args.length > 0 && _args[0] !== undefined ? _args[0] : {
            x: 23
          }, x = _ref.x;
          assert.sameValue(x, 23);
          callCount = callCount + 1;

        case 3:
        case "end":
          return _context.stop();
      }
    }
  }, f);
});
f().next();
assert.sameValue(callCount, 1, 'generator function invoked exactly once');