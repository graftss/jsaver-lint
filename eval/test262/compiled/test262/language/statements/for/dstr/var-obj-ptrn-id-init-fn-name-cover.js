// This file was procedurally generated from the following sources:
// - src/dstr-binding/obj-ptrn-id-init-fn-name-cover.case
// - src/dstr-binding/default/for-var.template

/*---
description: SingleNameBinding assigns `name` to "anonymous" functions "through" cover grammar (for statement)
esid: sec-for-statement-runtime-semantics-labelledevaluation
features: [destructuring-binding]
flags: [generated]
info: |
    IterationStatement :
        for ( var VariableDeclarationList ; Expressionopt ; Expressionopt ) Statement

    1. Let varDcl be the result of evaluating VariableDeclarationList.
    [...]

    13.3.2.4 Runtime Semantics: Evaluation

    VariableDeclarationList : VariableDeclarationList , VariableDeclaration

    1. Let next be the result of evaluating VariableDeclarationList.
    2. ReturnIfAbrupt(next).
    3. Return the result of evaluating VariableDeclaration.

    VariableDeclaration : BindingPattern Initializer

    1. Let rhs be the result of evaluating Initializer.
    2. Let rval be GetValue(rhs).
    3. ReturnIfAbrupt(rval).
    4. Return the result of performing BindingInitialization for BindingPattern
       passing rval and undefined as arguments.

    13.3.3.7 Runtime Semantics: KeyedBindingInitialization

    SingleNameBinding : BindingIdentifier Initializeropt

    [...]
    6. If Initializer is present and v is undefined, then
       [...]
       d. If IsAnonymousFunctionDefinition(Initializer) is true, then
          i. Let hasNameProperty be HasOwnProperty(v, "name").
          ii. ReturnIfAbrupt(hasNameProperty).
          iii. If hasNameProperty is false, perform SetFunctionName(v,
               bindingId).
---*/
var iterCount = 0;

for (var _ref = {}, _ref$cover = _ref.cover, cover = _ref$cover === void 0 ? function () {} : _ref$cover, _ref$xCover = _ref.xCover, xCover = _ref$xCover === void 0 ? (0, function () {}) : _ref$xCover; iterCount < 1;) {
  assert.sameValue(cover.name, 'cover');
  assert.notSameValue(xCover.name, 'xCover');
  iterCount += 1;
}

assert.sameValue(iterCount, 1, 'Iteration occurred as expected');