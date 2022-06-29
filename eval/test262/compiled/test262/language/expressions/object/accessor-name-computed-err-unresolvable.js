function _defineEnumerableProperties(obj, descs) { for (var key in descs) { var desc = descs[key]; desc.configurable = desc.enumerable = true; if ("value" in desc) desc.writable = true; Object.defineProperty(obj, key, desc); } if (Object.getOwnPropertySymbols) { var objectSymbols = Object.getOwnPropertySymbols(descs); for (var i = 0; i < objectSymbols.length; i++) { var sym = objectSymbols[i]; var desc = descs[sym]; desc.configurable = desc.enumerable = true; if ("value" in desc) desc.writable = true; Object.defineProperty(obj, sym, desc); } } return obj; }

// This file was procedurally generated from the following sources:
// - src/accessor-names/computed-err-unresolvable.case
// - src/accessor-names/error/obj.template

/*---
description: Abrupt completion when resolving reference value (Object initializer)
esid: sec-object-initializer-runtime-semantics-evaluation
flags: [generated]
info: |
    ObjectLiteral :
      { PropertyDefinitionList }
      { PropertyDefinitionList , }

    1. Let obj be ObjectCreate(%ObjectPrototype%).
    2. Let status be the result of performing PropertyDefinitionEvaluation of
       PropertyDefinitionList with arguments obj and true.

    12.2.6.7 Runtime Semantics: Evaluation

    [...]

    ComputedPropertyName : [ AssignmentExpression ]

    1. Let exprValue be the result of evaluating AssignmentExpression.
    2. Let propName be ? GetValue(exprValue).
---*/
assert["throws"](ReferenceError, function () {
  var _test262unresolvable;

  var _ref = {};
  _test262unresolvable = test262unresolvable;
  var _mutatorMap = {};
  _mutatorMap[_test262unresolvable] = _mutatorMap[_test262unresolvable] || {};

  _mutatorMap[_test262unresolvable].get = function () {};

  _defineEnumerableProperties(_ref, _mutatorMap);

  _ref;
}, '`get` accessor');
assert["throws"](ReferenceError, function () {
  var _test262unresolvable2;

  var _ref2 = {};
  _test262unresolvable2 = test262unresolvable;
  var _mutatorMap2 = {};
  _mutatorMap2[_test262unresolvable2] = _mutatorMap2[_test262unresolvable2] || {};

  _mutatorMap2[_test262unresolvable2].set = function (_) {};

  _defineEnumerableProperties(_ref2, _mutatorMap2);

  _ref2;
}, '`set` accessor');