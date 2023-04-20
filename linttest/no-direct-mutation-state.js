// placeholder React component class
class Component {
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

  initState(flag = true) {
    this.state = { x: 3 }
  }

  updateState() {
    this.state.x += 3;
  }
}

const a = new A();
a.initState();
a.updateState();