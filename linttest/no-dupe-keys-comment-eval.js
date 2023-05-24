function makeDupeKeyObj(key) {
  return { [key]: 3, [key]: 4 };
}

// lint-disable-eval
makeDupeKeyObj('x')