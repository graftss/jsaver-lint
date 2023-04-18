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

b(1);
b(false);
b(true);


