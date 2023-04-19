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

  componentDidMount(flag = true) {
    2;
    this.mySetState(flag, {});
  }

  componentDidUpdate() {
    2;
  }
}

const a = new A();
A.prototype.componentDidUpdate = A.prototype.componentDidMount;
a.componentDidUpdate();
a.componentDidUpdate(false);
a.componentDidMount();
