// placeholder React component class
class Component {
  setState() {}
  componentDidMount() {}
  componentDidUpdate() {}
  componentWillUpdate() {}
}

class A extends Component {
  componentDidMount() {
    this.setState();
  }
}

const a = new A();
a.componentDidMount();