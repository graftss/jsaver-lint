const table = { a: 1, b: 2, d: 4 };
function letterToNumber(letter) { return table[letter]; }
const result = ['a', 'b', 'c', 'd'].map(letterToNumber);
