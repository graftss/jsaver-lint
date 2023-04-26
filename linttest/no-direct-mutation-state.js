// placeholder React component class
class Component {
  constructor() {
    this.state = { x: 9, y: 5 }
  }
  setState() { 1; }
  componentDidMount() { 1; }
  componentDidUpdate() { 1; }
  componentWillUpdate() { 1; }
}

class A extends Component {
  constructor() {
    super()
  }

  mySetState(flag, value) {
    if (flag) {
      this.setState(value)
    }
  }

  updateState() {
    this.state.x += 3;
  }
}

const a = new A();
a.state = 3;