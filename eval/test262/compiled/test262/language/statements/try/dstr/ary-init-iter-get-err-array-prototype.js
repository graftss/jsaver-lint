function _slicedToArray(arr, i) { return _arrayWithHoles(arr) || _iterableToArrayLimit(arr, i) || _unsupportedIterableToArray(arr, i) || _nonIterableRest(); }

function _nonIterableRest() { throw new TypeError("Invalid attempt to destructure non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method."); }

function _unsupportedIterableToArray(o, minLen) { if (!o) return; if (typeof o === "string") return _arrayLikeToArray(o, minLen); var n = Object.prototype.toString.call(o).slice(8, -1); if (n === "Object" && o.constructor) n = o.constructor.name; if (n === "Map" || n === "Set") return Array.from(o); if (n === "Arguments" || /^(?:Ui|I)nt(?:8|16|32)(?:Clamped)?Array$/.test(n)) return _arrayLikeToArray(o, minLen); }

function _arrayLikeToArray(arr, len) { if (len == null || len > arr.length) len = arr.length; for (var i = 0, arr2 = new Array(len); i < len; i++) { arr2[i] = arr[i]; } return arr2; }

function _iterableToArrayLimit(arr, i) { var _i = arr == null ? null : typeof Symbol !== "undefined" && arr[Symbol.iterator] || arr["@@iterator"]; if (_i == null) return; var _arr = []; var _n = true; var _d = false; var _s, _e; try { for (_i = _i.call(arr); !(_n = (_s = _i.next()).done); _n = true) { _arr.push(_s.value); if (i && _arr.length === i) break; } } catch (err) { _d = true; _e = err; } finally { try { if (!_n && _i["return"] != null) _i["return"](); } finally { if (_d) throw _e; } } return _arr; }

function _arrayWithHoles(arr) { if (Array.isArray(arr)) return arr; }

// This file was procedurally generated from the following sources:
// - src/dstr-binding/ary-init-iter-get-err-array-prototype.case
// - src/dstr-binding/error/try.template

/*---
description: Abrupt completion returned by GetIterator (try statement)
esid: sec-runtime-semantics-catchclauseevaluation
features: [Symbol.iterator, destructuring-binding]
flags: [generated]
info: |
    Catch : catch ( CatchParameter ) Block

    [...]
    5. Let status be the result of performing BindingInitialization for
       CatchParameter passing thrownValue and catchEnv as arguments.
    [...]

    Runtime Semantics: BindingInitialization

    BindingPattern : ArrayBindingPattern

    1. Let iteratorRecord be ? GetIterator(value).

    GetIterator ( obj [ , hint [ , method ] ] )

    [...]
    4. Let iterator be ? Call(method, obj).

    Call ( F, V [ , argumentsList ] )

    [...]
    2. If IsCallable(F) is false, throw a TypeError exception.

---*/
delete Array.prototype[Symbol.iterator];
assert["throws"](TypeError, function () {
  try {
    throw [1, 2, 3];
  } catch (_ref) {
    var _ref2 = _slicedToArray(_ref, 3);

    var x = _ref2[0];
    var y = _ref2[1];
    var z = _ref2[2];
  }
});