var _this2 = this;

function _newArrowCheck(innerThis, boundThis) { if (innerThis !== boundThis) { throw new TypeError("Cannot instantiate an arrow function"); } }

// This file was procedurally generated from the following sources:
// - src/function-forms/reassign-fn-name-in-body-in-arrow.case
// - src/function-forms/expr-named/func-expr-named-strict-error.template

/*---
description: Reassignment of function name is silently ignored in non-strict mode code. (named function expression in strict mode code)
esid: sec-function-definitions-runtime-semantics-evaluation
flags: [generated, onlyStrict]
info: |
    FunctionExpression : function BindingIdentifier ( FormalParameters ) { FunctionBody }

---*/
// increment callCount in case "body"
var callCount = 0;

var ref = function BindingIdentifier() {
  var _this = this;

  callCount++;
  (function () {
    _newArrowCheck(this, _this);

    BindingIdentifier = 1;
  }).bind(this)();
  return BindingIdentifier;
};

assert["throws"](TypeError, function () {
  _newArrowCheck(this, _this2);

  ref();
}.bind(this));
assert.sameValue(callCount, 1, 'function invoked exactly once');