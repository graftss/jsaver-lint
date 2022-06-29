// This file was procedurally generated from the following sources:
// - src/function-forms/dflt-params-arg-val-undefined.case
// - src/function-forms/default/gen-meth.template

/*---
description: Use of initializer when argument value is `undefined` (generator method)
esid: sec-generator-function-definitions-runtime-semantics-propertydefinitionevaluation
features: [default-parameters, generators]
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


    14.1.19 Runtime Semantics: IteratorBindingInitialization

    FormalsList : FormalsList , FormalParameter

    [...]
    23. Let iteratorRecord be Record {[[Iterator]]:
        CreateListIterator(argumentsList), [[Done]]: false}.
    24. If hasDuplicates is true, then
        [...]
    25. Else,
        a. Perform ? IteratorBindingInitialization for formals with
           iteratorRecord and env as arguments.
    [...]

---*/
var callCount = 0;
var obj = {
  method: /*#__PURE__*/regeneratorRuntime.mark(function method() {
    var fromLiteral,
        fromExpr,
        fromHole,
        _args = arguments;
    return regeneratorRuntime.wrap(function method$(_context) {
      while (1) {
        switch (_context.prev = _context.next) {
          case 0:
            fromLiteral = _args.length > 0 && _args[0] !== undefined ? _args[0] : 23;
            fromExpr = _args.length > 1 && _args[1] !== undefined ? _args[1] : 45;
            fromHole = _args.length > 2 && _args[2] !== undefined ? _args[2] : 99;
            assert.sameValue(fromLiteral, 23);
            assert.sameValue(fromExpr, 45);
            assert.sameValue(fromHole, 99);
            callCount = callCount + 1;

          case 7:
          case "end":
            return _context.stop();
        }
      }
    }, method);
  })
};
obj.method(undefined, void 0).next(); // Stores a reference `ref` for case evaluation

var ref = obj.method;
assert.sameValue(callCount, 1, 'generator method invoked exactly once');