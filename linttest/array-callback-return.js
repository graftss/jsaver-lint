const xxx = 'asdf'
const yyy = '3'

function main() {
    const subtable = {x: 3, y: 4}
    const table = { a: 1, b: 2, d: subtable };
    const yyy = 78

    function letterToNumber(letter) {
      return table[letter];
    }

    function inner() {
        let xxx = 999;
        const keys = ['a', 'b', 'ccc', 'd']
        var ggg = 8;
        const result = keys.map(letterToNumber, table);

        table.ccc = 3;

        xxx = keys.map(letterToNumber);
    }

    inner()
}

function f(a) {
  if (a) main();
}

f(true);
f(false);