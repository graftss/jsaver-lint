// Copyright (C) 2018 Igalia, S.L. All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.

/*---
esid: sec-exp-operator-runtime-semantics-evaluation
description: Mixing BigInt and Number produces a TypeError for exponentiation operator
features: [BigInt]
info: |
  Let base be ? ToNumeric(leftValue).
  Let exponent be ? ToNumeric(rightValue).
  If Type(base) does not equal Type(exponent), throw a TypeError exception.
---*/
assert["throws"](TypeError, function () {
  Math.pow(1n, 1);
}, '1n ** 1 throws TypeError');
assert["throws"](TypeError, function () {
  Math.pow(1, 1n);
}, '1 ** 1n throws TypeError');
assert["throws"](TypeError, function () {
  Math.pow(Object(1n), 1);
}, 'Object(1n) ** 1 throws TypeError');
assert["throws"](TypeError, function () {
  Math.pow(1, Object(1n));
}, '1 ** Object(1n) throws TypeError');
assert["throws"](TypeError, function () {
  Math.pow(1n, Object(1));
}, '1n ** Object(1) throws TypeError');
assert["throws"](TypeError, function () {
  Math.pow(Object(1), 1n);
}, 'Object(1) ** 1n throws TypeError');
assert["throws"](TypeError, function () {
  Math.pow(Object(1n), Object(1));
}, 'Object(1n) ** Object(1) throws TypeError');
assert["throws"](TypeError, function () {
  Math.pow(Object(1), Object(1n));
}, 'Object(1) ** Object(1n) throws TypeError');
assert["throws"](TypeError, function () {
  Math.pow(1n, NaN);
}, '1n ** NaN throws TypeError');
assert["throws"](TypeError, function () {
  Math.pow(NaN, 1n);
}, 'NaN ** 1n throws TypeError');
assert["throws"](TypeError, function () {
  Math.pow(1n, Infinity);
}, '1n ** Infinity throws TypeError');
assert["throws"](TypeError, function () {
  Math.pow(Infinity, 1n);
}, 'Infinity ** 1n throws TypeError');
assert["throws"](TypeError, function () {
  Math.pow(1n, true);
}, '1n ** true throws TypeError');
assert["throws"](TypeError, function () {
  Math.pow(true, 1n);
}, 'true ** 1n throws TypeError');
assert["throws"](TypeError, function () {
  Math.pow(1n, '1');
}, '1n ** "1" throws TypeError');
assert["throws"](TypeError, function () {
  Math.pow('1', 1n);
}, '"1" ** 1n throws TypeError');
assert["throws"](TypeError, function () {
  Math.pow(1n, null);
}, '1n ** null throws TypeError');
assert["throws"](TypeError, function () {
  Math.pow(null, 1n);
}, 'null ** 1n throws TypeError');
assert["throws"](TypeError, function () {
  Math.pow(1n, undefined);
}, '1n ** undefined throws TypeError');
assert["throws"](TypeError, function () {
  Math.pow(undefined, 1n);
}, 'undefined ** 1n throws TypeError');