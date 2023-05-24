function makeDupeKeyObj(key) {
  // lint-disable-stmt
  return { [key]: 3, [key]: 4 };
}

function makeDupeKeyObjMaker(key) {
  return function() {
    return makeDupeKeyObj(key)
  }
}

// lint-disable-stmt
makeDupeKeyObjMaker('x')()