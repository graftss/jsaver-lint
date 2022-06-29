// This file was procedurally generated from the following sources:
// - src/dstr-binding/obj-ptrn-prop-obj-value-undef.case
// - src/dstr-binding/error/gen-meth.template

/*---
description: Object binding pattern with "nested" object binding pattern taking the `null` value (generator method)
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

    [...]
    3. If Initializer is present and v is undefined, then
       [...]
    4. Return the result of performing BindingInitialization for BindingPattern
       passing v and environment as arguments.
---*/
var obj = {
  method: /*#__PURE__*/regeneratorRuntime.mark(function method(_ref) {
    var _ref$w, x, y, z;

    return regeneratorRuntime.wrap(function method$(_context) {
      while (1) {
        switch (_context.prev = _context.next) {
          case 0:
            _ref$w = _ref.w;
            _ref$w = _ref$w === void 0 ? undefined : _ref$w;
            x = _ref$w.x, y = _ref$w.y, z = _ref$w.z;

          case 3:
          case "end":
            return _context.stop();
        }
      }
    }, method);
  })
};
assert["throws"](TypeError, function () {
  obj.method({});
});