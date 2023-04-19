function c() {}

function a(x) {
  if (typeof x == 'number') {
    c()
  } else {
    a(1)
  }
}

function b(x) {
  if (x) {
    a(x)
  }
}

var x = b;

x(1);
x(false);
x(true);


