class Component {
  setState() { 1; }
  componentDidMount() { 1; }
  componentDidUpdate() { 1; }
  componentWillUpdate() { 1; }
}

class MyComponent extends Component {
  componentDidUpdate() { this.updateState(); }
  updateState() { this.setState({ x: 3 }); }
}

const a = new MyComponent()
a.componentDidUpdate()