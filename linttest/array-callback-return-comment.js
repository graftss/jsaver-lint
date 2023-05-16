// lint-disable-next array-callback-return
function noReturn() { return; }

function noReturn2() { return; }

const a = [1, 2]
a.map(noReturn)
a.map(noReturn2)