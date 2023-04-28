const table = { a: 1, b: 2, d: 4 };

function letterToNumber(letter) {
  return table[letter];
}

const keys = ['a', 'b', 'c', 'd']
const result = keys.map(letterToNumber);

table.c = 3;

const result2 = keys.map(letterToNumber);