function _defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } }

function _createClass(Constructor, protoProps, staticProps) { if (protoProps) _defineProperties(Constructor.prototype, protoProps); if (staticProps) _defineProperties(Constructor, staticProps); Object.defineProperty(Constructor, "prototype", { writable: false }); return Constructor; }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

// This file was procedurally generated from the following sources:
// - src/dstr-binding/obj-ptrn-id-init-fn-name-class.case
// - src/dstr-binding/default/func-decl-dflt.template

/*---
description: SingleNameBinding assigns `name` to "anonymous" classes (function declaration (default parameter))
esid: sec-function-definitions-runtime-semantics-instantiatefunctionobject
features: [destructuring-binding, default-parameters]
flags: [generated]
info: |
    FunctionDeclaration :
        function BindingIdentifier ( FormalParameters ) { FunctionBody }

        [...]
        3. Let F be FunctionCreate(Normal, FormalParameters, FunctionBody,
           scope, strict).
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
       d. If IsAnonymousFunctionDefinition(Initializer) is true, then
          i. Let hasNameProperty be HasOwnProperty(v, "name").
          ii. ReturnIfAbrupt(hasNameProperty).
          iii. If hasNameProperty is false, perform SetFunctionName(v,
               bindingId).
---*/
var callCount = 0;

function f() {
  var _ref = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : {},
      _ref$cls = _ref.cls,
      cls = _ref$cls === void 0 ? /*#__PURE__*/function () {
    function _class() {
      _classCallCheck(this, _class);
    }

    return _createClass(_class);
  }() : _ref$cls,
      _ref$xCls = _ref.xCls,
      xCls = _ref$xCls === void 0 ? /*#__PURE__*/_createClass(function X() {
    _classCallCheck(this, X);
  }) : _ref$xCls,
      _ref$xCls2 = _ref.xCls2,
      xCls2 = _ref$xCls2 === void 0 ? /*#__PURE__*/function () {
    function _class2() {
      _classCallCheck(this, _class2);
    }

    _createClass(_class2, null, [{
      key: "name",
      value: function name() {}
    }]);

    return _class2;
  }() : _ref$xCls2;

  assert.sameValue(cls.name, 'cls');
  assert.notSameValue(xCls.name, 'xCls');
  assert.notSameValue(xCls2.name, 'xCls2');
  callCount = callCount + 1;
}

;
f();
assert.sameValue(callCount, 1, 'function invoked exactly once');