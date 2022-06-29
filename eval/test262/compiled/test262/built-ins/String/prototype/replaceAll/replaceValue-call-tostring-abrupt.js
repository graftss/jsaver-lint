// Copyright (C) 2019 Leo Balter. All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.

/*---
esid: sec-string.prototype.replaceall
description: >
  Return abrupt from ToString(Call(replaceValue, ...))
info: |
  String.prototype.replaceAll ( searchValue, replaceValue )

  ...
  5. Let functionalReplace be IsCallable(replaceValue).
  ...
  14. For each position in matchPositions, do
    a. If functionalReplace is true, then
      i. Let replacement be ? ToString(? Call(replaceValue, undefined, « searchString, position, string »).
features: [String.prototype.replaceAll, Symbol]
---*/
function custom() {
  return {
    toString: function toString() {
      throw new Test262Error();
    }
  };
}

assert["throws"](Test262Error, function () {
  'a'.replaceAll('a', custom);
});

function symbol() {
  return {
    toString: function toString() {
      return Symbol();
    }
  };
}

assert["throws"](TypeError, function () {
  'a'.replaceAll('a', symbol);
});