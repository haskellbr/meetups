import 'codemirror/lib/codemirror.css';
import 'codemirror/mode/haskell/haskell';
import 'codemirror/theme/3024-day.css';
import 'codemirror/theme/3024-night.css';
import 'react-console-component/src/react-console.scss';
import 'react-grid-layout/css/styles.css';
import Codemirror from 'react-codemirror';
import Console from 'react-console-component';
import React, {Component} from 'react';
import {Responsive, WidthProvider} from 'react-grid-layout'

const Grid = WidthProvider(Responsive);

import './App.css';

class App extends Component {
  onSubmitRepl = (text) => {
    if (!text.replace(/\s+$/, '').length) {
      this.refs.console.return();
      return;
    }
    this.ws.send({
      type: 'EVAL',
      payload: text,
    });
  };

  onMessage = (event) => {
    console.log('<-', event.data);
    if (event.data === '<DONE>') {
      this.refs.console.return();
      return;
    }

    if (event.data && this.refs.console.state.log.length) {
      this.refs.console.log(event.data.split('\n')[0].replace(/^> /, ''));
      this.refs.console.return();
      return;
    }

    this.refs.console.return();
  };

  componentDidMount() {
    this.mounted = true;
    this.ws = new WebSocket('ws://localhost:9160');
    this.ws.onmessage = this.onMessage;
  }

  onClickLoad = () => {};

  onClickClear = () => {
    this.refs.console.setState({
      history: [],
      acceptInput: true,
      historyn: 0,
      kill: [],
      log: [],
    });
  };

  onClickRun = () => {};

  render() {
    const layout = [
      {
        i: 'editor',
        x: 0,
        y: 0,
        w: 6,
        minW: 3,
        maxW: 12,
        h: 12,
      },
      {
        i: 'repl',
        x: 6,
        y: 0,
        w: 6,
        minW: 3,
        maxW: 12,
        h: 12,
      },
      {
        i: 'controls',
        x: 0,
        y: 0,
        w: 12,
        minW: 3,
        maxW: 12,
        h: 1.2,
      },
    ];

    return (
      <Grid
        className="App react-grid-layout"
        layouts={{lg: layout, md: layout}}
        cols={{lg: 12, md: 12, sm: 12, xs: 12, xxs: 12}}
        measureBeforeMount={false}
        rowHeight={30}
        useCSSTransforms
      >
        <div key="controls">
          <button onClick={this.onClickLoad}>Load</button>
          <button onClick={this.onClickClear}>Clear</button>
          <button onClick={this.onClickRun}>Run</button>
        </div>

        <div
          key="editor"
          onKeyDown={(e) => {
            if (e.ctrlKey && e.key === 'r') {
              this.ws.send({
                type: 'RUN',
                payload: this.state.value,
              })
            }
          }}
        >
          <Codemirror
            value={this.state && this.state.value}
            onChange={(t) => this.setState({ value: t })}
            options={{
              theme: '3024-day',
              mode: 'haskell',
              lineNumbers: true,
            }}
          />
        </div>

        <div key="repl">
          <Console
            ref="console"
            handler={this.onSubmitRepl}
            promptLabel="ghci> "
          />
        </div>
      </Grid>
    );
  }
}

export default App;
