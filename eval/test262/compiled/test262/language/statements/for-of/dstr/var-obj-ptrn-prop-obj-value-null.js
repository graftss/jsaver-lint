// This file was procedurally generated from the following sources:
// - src/dstr-binding/obj-ptrn-prop-obj-value-null.case
// - src/dstr-binding/error/for-of-var.template

/*---
description: Object binding pattern with "nested" object binding pattern taking the `null` value (for-of statement)
esid: sec-for-in-and-for-of-statements-runtime-semantics-labelledevaluation
features: [destructuring-binding]
flags: [generated]
info: |
    IterationStatement :
        for ( var ForBinding of AssignmentExpression ) Statement

    [...]
    3. Return ForIn/OfBodyEvaluation(ForBinding, Statement, keyResult,
       varBinding, labelSet).

    13.7.5.13 Runtime Semantics: ForIn/OfBodyEvaluation

    [...]
    3. Let destructuring be IsDestructuring of lhs.
    [...]
    5. Repeat
       [...]
       h. If destructuring is false, then
          [...]
       i. Else
          i. If lhsKind is assignment, then
             [...]
          ii. Else if lhsKind is varBinding, then
              1. Assert: lhs is a ForBinding.
              2. Let status be the result of performing BindingInitialization
                 for lhs passing nextValue and undefined as the arguments.
          [...]

    13.3.3.7 Runtime Semantics: KeyedBindingInitialization

    [...]
    3. If Initializer is present and v is undefined, then
       [...]
    4. Return the result of performing BindingInitialization for BindingPattern
       passing v and environment as arguments.
---*/
assert["throws"](TypeError, function () {
  for (var _i = 0, _arr = [{
    w: null
  }]; _i < _arr.length; _i++) {
    var _arr$_i$w = _arr[_i].w;
    _arr$_i$w = _arr$_i$w === void 0 ? {
      x: 4,
      y: 5,
      z: 6
    } : _arr$_i$w;
    var x = _arr$_i$w.x,
        y = _arr$_i$w.y,
        z = _arr$_i$w.z;
    return;
  }
});