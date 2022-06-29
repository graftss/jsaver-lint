var _this = this;

function _newArrowCheck(innerThis, boundThis) { if (innerThis !== boundThis) { throw new TypeError("Cannot instantiate an arrow function"); } }

// This file was procedurally generated from the following sources:
// - src/function-forms/reassign-fn-name-in-body-in-eval.case
// - src/function-forms/expr-named/gen-func-expr-named-strict-error.template

/*---
description: Reassignment of function name is silently ignored in non-strict mode code. (named generator function expression in strict mode code)
esid: sec-generator-function-definitions-runtime-semantics-evaluation
features: [generators]
flags: [generated, onlyStrict]
info: |
    GeneratorExpression : function * BindingIdentifier ( FormalParameters ) { GeneratorBody }

---*/
// increment callCount in case "body"
var callCount = 0;
var ref = /*#__PURE__*/regeneratorRuntime.mark(function BindingIdentifier() {
  return regeneratorRuntime.wrap(function BindingIdentifier$(_context) {
    while (1) {
      switch (_context.prev = _context.next) {
        case 0:
          callCount++;
          eval("BindingIdentifier = 1");
          return _context.abrupt("return", BindingIdentifier);

        case 3:
        case "end":
          return _context.stop();
      }
    }
  }, BindingIdentifier);
});
assert["throws"](TypeError, function () {
  _newArrowCheck(this, _this);

  ref().next();
}.bind(this));
assert.sameValue(callCount, 1, 'function invoked exactly once');