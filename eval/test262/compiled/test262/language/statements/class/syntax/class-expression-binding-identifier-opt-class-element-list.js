function _typeof(obj) { "@babel/helpers - typeof"; return _typeof = "function" == typeof Symbol && "symbol" == typeof Symbol.iterator ? function (obj) { return typeof obj; } : function (obj) { return obj && "function" == typeof Symbol && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : typeof obj; }, _typeof(obj); }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } }

function _createClass(Constructor, protoProps, staticProps) { if (protoProps) _defineProperties(Constructor.prototype, protoProps); if (staticProps) _defineProperties(Constructor, staticProps); Object.defineProperty(Constructor, "prototype", { writable: false }); return Constructor; }

// Copyright (C) 2015 the V8 project authors. All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.

/*---
es6id: 14.5
description: >
    ClassExpression[Yield,GeneratorParameter] :
      class BindingIdentifier[?Yield]opt ClassTail[?Yield,?GeneratorParameter]

    ClassDeclaration:
      class BindingIdentifier ClassTail

    ClassTail:
      ... { ClassBodyopt }

    ClassBody[Yield] :
      ClassElementList[?Yield]


    ClassElementList[Yield] :
      ClassElement[?Yield]
      ClassElementList[?Yield] ClassElement[?Yield]

    ClassElement[Yield] :
      MethodDefinition[?Yield]
      static MethodDefinition[?Yield]
      ;

---*/
var A = /*#__PURE__*/function () {
  function B() {
    _classCallCheck(this, B);
  }

  _createClass(B, [{
    key: "method",
    value: function method() {}
  }], [{
    key: "method",
    value: function method() {}
  }]);

  return B;
}();

assert.sameValue(_typeof(A), "function");
assert.sameValue(_typeof(A.prototype.method), "function");
assert.sameValue(_typeof(A.method), "function");
assert.sameValue(typeof B === "undefined" ? "undefined" : _typeof(B), "undefined");