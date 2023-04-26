// placeholder React component class
class Component {
  constructor() {
  }
  setState() { 1; }
  componentDidMount() { 1; }
  componentDidUpdate() { 1; }
  componentWillUpdate() { 1; }
}

class A extends Component {
  constructor() {
    super()
    this.state = { x: 9 }
  }
}

const a = new A();

a.state.x = {};
a.state.x.z = 9;
