function main(a, b) {
    let x = 0;
    const methods = ['forEach', 'map', 'filter'];
    const callbacks = [function(a) { x += a; }, a => a * 2, a => a > 3];
    const arr = [2, 3, 4, 5];

    methods.forEach((methodName, idx) => {
      arr[methodName](callbacks[idx])
    })
}

main();