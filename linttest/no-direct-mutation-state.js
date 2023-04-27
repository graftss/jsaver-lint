// placeholder React component class
class Component {
  constructor() { }
  setState() { }
  componentDidMount() { }
  componentDidUpdate() { }
  componentWillUpdate() { }
}

class A extends Component {
  constructor() {
    super()
    this.state = { x: 9, y: {} }
  }

  badUpdateState(flag, prop) {
    if (flag) {
      this.state.y[prop] = true;
    }
  }
}

function main() {
  const a = new A();
  a.badUpdateState(false, "x");
  a.badUpdateState(true, "abc");
}

main();
