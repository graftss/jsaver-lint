function e() {
  return 0;
}

function f() {
  return e();
}

function g(x) {
  if (x) {
    return f();
  }  else {
    return e();
  }
}

g(true);
g(false);
//g();
//g();
//g();