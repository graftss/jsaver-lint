function _defineProperty(obj, key, value) { if (key in obj) { Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true }); } else { obj[key] = value; } return obj; }

function _typeof(obj) { "@babel/helpers - typeof"; return _typeof = "function" == typeof Symbol && "symbol" == typeof Symbol.iterator ? function (obj) { return typeof obj; } : function (obj) { return obj && "function" == typeof Symbol && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : typeof obj; }, _typeof(obj); }

// Copyright (C) 2017 Josh Wolfe. All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.

/*---
description: BigInt.asUintN type coercion for bigint parameter
esid: pending
info: |
  BigInt.asUintN ( bits, bigint )

  2. Let bigint ? ToBigInt(bigint).
features: [BigInt, computed-property-names, Symbol, Symbol.toPrimitive]
---*/
assert.sameValue(typeof BigInt === "undefined" ? "undefined" : _typeof(BigInt), 'function');
assert.sameValue(_typeof(BigInt.asUintN), 'function');
assert["throws"](TypeError, function () {
  BigInt.asUintN();
}, "ToBigInt: no argument => undefined => TypeError");
assert["throws"](TypeError, function () {
  BigInt.asUintN(0);
}, "ToBigInt: no argument => undefined => TypeError");
assert["throws"](TypeError, function () {
  BigInt.asUintN(0, undefined);
}, "ToBigInt: undefined => TypeError");
assert["throws"](TypeError, function () {
  BigInt.asUintN(0, _defineProperty({}, Symbol.toPrimitive, function () {
    return undefined;
  }));
}, "ToBigInt: @@toPrimitive => undefined => TypeError");
assert["throws"](TypeError, function () {
  BigInt.asUintN(0, {
    valueOf: function valueOf() {
      return undefined;
    }
  });
}, "ToBigInt: valueOf => undefined => TypeError");
assert["throws"](TypeError, function () {
  BigInt.asUintN(0, {
    toString: function toString() {
      return undefined;
    }
  });
}, "ToBigInt: toString => undefined => TypeError");
assert["throws"](TypeError, function () {
  BigInt.asUintN(0, null);
}, "ToBigInt: null => TypeError");
assert["throws"](TypeError, function () {
  BigInt.asUintN(0, _defineProperty({}, Symbol.toPrimitive, function () {
    return null;
  }));
}, "ToBigInt: @@toPrimitive => null => TypeError");
assert["throws"](TypeError, function () {
  BigInt.asUintN(0, {
    valueOf: function valueOf() {
      return null;
    }
  });
}, "ToBigInt: valueOf => null => TypeError");
assert["throws"](TypeError, function () {
  BigInt.asUintN(0, {
    toString: function toString() {
      return null;
    }
  });
}, "ToBigInt: toString => null => TypeError");
assert["throws"](TypeError, function () {
  BigInt.asUintN(0, 0);
}, "ToBigInt: Number => TypeError");
assert["throws"](TypeError, function () {
  BigInt.asUintN(0, Object(0));
}, "ToBigInt: unbox object with internal slot => Number => TypeError");
assert["throws"](TypeError, function () {
  BigInt.asUintN(0, _defineProperty({}, Symbol.toPrimitive, function () {
    return 0;
  }));
}, "ToBigInt: @@toPrimitive => Number => TypeError");
assert["throws"](TypeError, function () {
  BigInt.asUintN(0, {
    valueOf: function valueOf() {
      return 0;
    }
  });
}, "ToBigInt: valueOf => Number => TypeError");
assert["throws"](TypeError, function () {
  BigInt.asUintN(0, {
    toString: function toString() {
      return 0;
    }
  });
}, "ToBigInt: toString => Number => TypeError");
assert["throws"](TypeError, function () {
  BigInt.asUintN(0, NaN);
}, "ToBigInt: Number => TypeError");
assert["throws"](TypeError, function () {
  BigInt.asUintN(0, Infinity);
}, "ToBigInt: Number => TypeError");
assert["throws"](TypeError, function () {
  BigInt.asUintN(0, Symbol("1"));
}, "ToBigInt: Symbol => TypeError");
assert["throws"](TypeError, function () {
  BigInt.asUintN(0, Object(Symbol("1")));
}, "ToBigInt: unbox object with internal slot => Symbol => TypeError");
assert["throws"](TypeError, function () {
  BigInt.asUintN(0, _defineProperty({}, Symbol.toPrimitive, function () {
    return Symbol("1");
  }));
}, "ToBigInt: @@toPrimitive => Symbol => TypeError");
assert["throws"](TypeError, function () {
  BigInt.asUintN(0, {
    valueOf: function valueOf() {
      return Symbol("1");
    }
  });
}, "ToBigInt: valueOf => Symbol => TypeError");
assert["throws"](TypeError, function () {
  BigInt.asUintN(0, {
    toString: function toString() {
      return Symbol("1");
    }
  });
}, "ToBigInt: toString => Symbol => TypeError");
assert["throws"](SyntaxError, function () {
  BigInt.asUintN(0, "a");
}, "ToBigInt: unparseable BigInt");
assert["throws"](SyntaxError, function () {
  BigInt.asUintN(0, "0b2");
}, "ToBigInt: unparseable BigInt binary");
assert["throws"](SyntaxError, function () {
  BigInt.asUintN(0, Object("0b2"));
}, "ToBigInt: unbox object with internal slot => unparseable BigInt binary");
assert["throws"](SyntaxError, function () {
  BigInt.asUintN(0, _defineProperty({}, Symbol.toPrimitive, function () {
    return "0b2";
  }));
}, "ToBigInt: @@toPrimitive => unparseable BigInt binary");
assert["throws"](SyntaxError, function () {
  BigInt.asUintN(0, {
    valueOf: function valueOf() {
      return "0b2";
    }
  });
}, "ToBigInt: valueOf => unparseable BigInt binary");
assert["throws"](SyntaxError, function () {
  BigInt.asUintN(0, {
    toString: function toString() {
      return "0b2";
    }
  });
}, "ToBigInt: toString => unparseable BigInt binary");
assert["throws"](SyntaxError, function () {
  BigInt.asUintN(0, "   0b2   ");
}, "ToBigInt: unparseable BigInt with leading/trailing whitespace");
assert["throws"](SyntaxError, function () {
  BigInt.asUintN(0, "0o8");
}, "ToBigInt: unparseable BigInt octal");
assert["throws"](SyntaxError, function () {
  BigInt.asUintN(0, "0xg");
}, "ToBigInt: unparseable BigInt hex");
assert["throws"](SyntaxError, function () {
  BigInt.asUintN(0, "1n");
}, "ToBigInt: unparseable BigInt due to literal suffix");