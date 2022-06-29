// This file was procedurally generated from the following sources:
// - src/arguments/args-trailing-comma-null.case
// - src/arguments/default/gen-meth.template

/*---
description: A trailing comma after null should not increase the arguments.length (generator method)
esid: sec-arguments-exotic-objects
features: [generators]
flags: [generated]
info: |
    9.4.4 Arguments Exotic Objects

    Most ECMAScript functions make an arguments object available to their code. Depending upon the
    characteristics of the function definition, its arguments object is either an ordinary object
    or an arguments exotic object.


    Trailing comma in the arguments list

    Left-Hand-Side Expressions

    Arguments :
        ( )
        ( ArgumentList )
        ( ArgumentList , )

    ArgumentList :
        AssignmentExpression
        ... AssignmentExpression
        ArgumentList , AssignmentExpression
        ArgumentList , ... AssignmentExpression
---*/
var callCount = 0;
var obj = {
  method: /*#__PURE__*/regeneratorRuntime.mark(function method() {
    var _args = arguments;
    return regeneratorRuntime.wrap(function method$(_context) {
      while (1) {
        switch (_context.prev = _context.next) {
          case 0:
            assert.sameValue(_args.length, 2);
            assert.sameValue(_args[0], 42);
            assert.sameValue(_args[1], null);
            callCount = callCount + 1;

          case 4:
          case "end":
            return _context.stop();
        }
      }
    }, method);
  })
};
obj.method(42, null).next(); // Stores a reference `ref` for case evaluation

var ref = obj.method;
assert.sameValue(callCount, 1, 'generator method invoked exactly once');