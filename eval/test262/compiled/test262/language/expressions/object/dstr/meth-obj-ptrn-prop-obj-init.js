// This file was procedurally generated from the following sources:
// - src/dstr-binding/obj-ptrn-prop-obj-init.case
// - src/dstr-binding/default/meth.template

/*---
description: Object binding pattern with "nested" object binding pattern using initializer (method)
esid: sec-runtime-semantics-definemethod
features: [destructuring-binding]
flags: [generated]
info: |
    MethodDefinition : PropertyName ( StrictFormalParameters ) { FunctionBody }

    [...]
    6. Let closure be FunctionCreate(kind, StrictFormalParameters,
       FunctionBody, scope, strict). If functionPrototype was passed as a
       parameter then pass its value as the functionPrototype optional argument
       of FunctionCreate.
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
       a. Let defaultValue be the result of evaluating Initializer.
       b. Let v be GetValue(defaultValue).
       c. ReturnIfAbrupt(v).
    4. Return the result of performing BindingInitialization for BindingPattern
       passing v and environment as arguments.
---*/
var callCount = 0;
var obj = {
  method: function method(_ref) {
    var _ref$w = _ref.w;
    _ref$w = _ref$w === void 0 ? {
      x: 4,
      y: 5,
      z: 6
    } : _ref$w;
    var x = _ref$w.x,
        y = _ref$w.y,
        z = _ref$w.z;
    assert.sameValue(x, 4);
    assert.sameValue(y, 5);
    assert.sameValue(z, 6);
    assert["throws"](ReferenceError, function () {
      w;
    });
    callCount = callCount + 1;
  }
};
obj.method({
  w: undefined
});
assert.sameValue(callCount, 1, 'method invoked exactly once');