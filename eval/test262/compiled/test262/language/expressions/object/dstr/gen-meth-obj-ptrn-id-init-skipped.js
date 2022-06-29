// This file was procedurally generated from the following sources:
// - src/dstr-binding/obj-ptrn-id-init-skipped.case
// - src/dstr-binding/default/gen-meth.template

/*---
description: Destructuring initializer is not evaluated when value is not `undefined` (generator method)
esid: sec-generator-function-definitions-runtime-semantics-propertydefinitionevaluation
features: [generators, destructuring-binding]
flags: [generated]
info: |
    GeneratorMethod :
        * PropertyName ( StrictFormalParameters ) { GeneratorBody }

    1. Let propKey be the result of evaluating PropertyName.
    2. ReturnIfAbrupt(propKey).
    3. If the function code for this GeneratorMethod is strict mode code,
       let strict be true. Otherwise let strict be false.
    4. Let scope be the running execution context's LexicalEnvironment.
    5. Let closure be GeneratorFunctionCreate(Method,
       StrictFormalParameters, GeneratorBody, scope, strict).
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
var obj = {
  method: /*#__PURE__*/regeneratorRuntime.mark(function method(_ref) {
    var _ref$w, w, _ref$x, x, _ref$y, y, _ref$z, z;

    return regeneratorRuntime.wrap(function method$(_context) {
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
    }, method);
  })
};
obj.method({
  w: null,
  x: 0,
  y: false,
  z: ''
}).next();
assert.sameValue(callCount, 1, 'generator method invoked exactly once');