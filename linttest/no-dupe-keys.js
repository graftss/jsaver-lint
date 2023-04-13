function makeObject(key1, key2) {
    return { [key1]: 3, y: 9, [key2]: 7 };
}

const obj1 = makeObject('x', 'z');
const obj2 = makeObject('y', 'z');
const obj3 = makeObject('x', 'x');