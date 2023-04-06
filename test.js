function e() {
  var x = function () {};

  return 0;
}

function f() {
  return e();
}

function g(x) {
  if (x > 3) {
    return f();
  }  else {
    return e();
  }
}

for (let i = 0; i < 6; i++) {
  g(i)
}
//g();
//g();