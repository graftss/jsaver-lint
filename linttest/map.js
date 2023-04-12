const arr = [1, 2, 3];

let x = 0;
function double(item) {
  x += item;
  if (x > 1) {
    return item * 2
  }
}

const result = arr.map(double);