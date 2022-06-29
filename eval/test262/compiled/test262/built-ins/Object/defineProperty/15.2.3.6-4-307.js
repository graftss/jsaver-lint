// Copyright (c) 2012 Ecma International.  All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.

/*---
es5id: 15.2.3.6-4-307
description: >
    Object.defineProperty - 'O' is an Arguments object, 'name' is an
    index named data property of 'O' but not defined in
    [[ParameterMap]] of 'O', test TypeError is thrown when updating
    the [[Enumerable]] attribute value of 'name' which is not
    configurable (10.6 [[DefineOwnProperty]] step 4)
includes: [propertyHelper.js]
---*/
(function () {
  Object.defineProperty(arguments, "0", {
    value: 0,
    writable: false,
    enumerable: true,
    configurable: false
  });

  try {
    Object.defineProperty(arguments, "0", {
      enumerable: false
    });
    $ERROR("Expected an exception.");
  } catch (e) {
    verifyEqualTo(arguments, "0", 0);
    verifyNotWritable(arguments, "0");
    verifyEnumerable(arguments, "0");
    verifyNotConfigurable(arguments, "0");

    if (!(e instanceof TypeError)) {
      $ERROR("Expected TypeError, got " + e);
    }
  }
})();