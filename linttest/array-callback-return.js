const xxx = 'asdf'
const yyy = '3'

function main() {

    const table = { a: 1, b: 2, d: 4 };
    const yyy = 78

    function letterToNumber(letter) {
      return table[letter];
    }

    function inner() {
        let xxx = 999;
        const keys = ['a', 'b', 'ccc', 'd']
        const result = keys.map(letterToNumber);

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