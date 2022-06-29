function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } }

function _createClass(Constructor, protoProps, staticProps) { if (protoProps) _defineProperties(Constructor.prototype, protoProps); if (staticProps) _defineProperties(Constructor, staticProps); Object.defineProperty(Constructor, "prototype", { writable: false }); return Constructor; }

// This file was procedurally generated from the following sources:
// - src/computed-property-names/computed-property-name-from-expression-logical-and.case
// - src/computed-property-names/evaluation/class-declaration.template

/*---
description: Computed property name from logical and (ComputedPropertyName in ClassDeclaration)
esid: prod-ComputedPropertyName
features: [computed-property-names]
flags: [generated]
info: |
    ClassExpression:
      classBindingIdentifier opt ClassTail

    ClassTail:
      ClassHeritage opt { ClassBody opt }

    ClassBody:
      ClassElementList

    ClassElementList:
      ClassElement

    ClassElement:
      MethodDefinition

    MethodDefinition:
      PropertyName ...
      get PropertyName ...
      set PropertyName ...

    PropertyName:
      ComputedPropertyName

    ComputedPropertyName:
      [ AssignmentExpression ]
---*/
var x = 0;

var C = /*#__PURE__*/function () {
  function C() {
    _classCallCheck(this, C);
  }

  _createClass(C, [{
    key: x && 1,
    value: function value() {
      return 2;
    }
  }], [{
    key: x && 1,
    value: function value() {
      return 2;
    }
  }]);

  return C;
}();

;
var c = new C();
assert.sameValue(c[x && 1](), 2);
assert.sameValue(C[x && 1](), 2);
assert.sameValue(c[String(x && 1)](), 2);
assert.sameValue(C[String(x && 1)](), 2);
assert.sameValue(x, 0);