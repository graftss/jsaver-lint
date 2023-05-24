function makeObject(key1, key2) {
    // lint-disable-eval no-dupe-keys
    return { [key1]: 3, y: 9, [key2]: 7 };
}

function main() {
    const obj1 = makeObject('x', 'z');
    const obj2 = makeObject('y', 'z');
    const obj3 = makeObject('x', 'x');

    const result = obj1.x + obj1.y + obj1.z;
}

main()

