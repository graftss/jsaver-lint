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
    this.state = { x: 9, y: { z: 88} }
  }
}

const a = new A();

a.state.x = 9;
a.state.y.z = 3;
