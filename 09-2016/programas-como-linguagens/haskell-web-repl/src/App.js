import 'codemirror/lib/codemirror.css';
import 'codemirror/mode/haskell/haskell';
import 'codemirror/theme/3024-day.css';
import 'codemirror/theme/3024-night.css';
import 'react-console-component/src/react-console.scss';
import 'react-grid-layout/css/styles.css';
import Codemirror from 'react-codemirror';
import Console from 'react-console-component';
import React, {Component} from 'react';
import {Responsive, WidthProvider} from 'react-grid-layout';
import ReconnectingWebSocket from 'reconnecting-websocket';

const Grid = WidthProvider(Responsive);

import './App.css';

class App extends Component {
  constructor(props) {
    super(props);
    this.state = {
      messages: [],
      logs: [],
    };
  }

  onSubmitRepl = (text) => {
    if (!text.replace(/\s+$/, '').length) {
      this.refs.console.return();
      return;
    }

    this.send({
      type: 'EVAL',
      payload: text,
    });
  };

  send(message) {
    console.log('->', JSON.stringify(message));
    this.ws.send(JSON.stringify(message));
  }

  run() {
    this.setState({ logs: [] });
    this.send({
      type: 'RUN_AUTO',
      payload: this.state.value,
    });
  }

  load() {
    this.send({
      type: 'LOAD_FILE',
      payload: this.state.value,
    });
  }

  onMessage = (event) => {
    let json;
    try {
      json = JSON.parse(event.data);
    } catch (err) {
      console.error(err);
      return;
    }

    console.log('<-', event.data);
    if (json.type === 'EVAL_DONE') {
      this.refs.console.return();
      return;
    }

    if (json.type === 'EVAL_LOG' && json.payload && this.refs.console.state.log.length) {
      this.refs.console.log(json.payload.split('\n')[0].replace(/^> /, ''));
      this.refs.console.return();
      return;
    }

    let logs = this.state.logs;
    if (json.type === 'RUN_COMPILE_LOG') {
      logs = logs.concat({
        text: json.payload,
        className: 'compile',
      });
    }

    if (json.type === 'RUN_COMMAND') {
      logs = logs.concat({
        text: json.payload,
        className: 'command ' + json.handle,
      });
    }

    if (json.type === 'RUN_LOG') {
      logs = logs.concat({
        text: json.payload,
        className: 'run ' + json.handle,
      });
    }

    this.setState({
      messages: this.state.messages.concat([json]),
      logs,
    });

    this.refs.console.return();
  };

  componentDidMount() {
    this.mounted = true;
    this.ws = new ReconnectingWebSocket('ws://localhost:9160');
    this.ws.onmessage = this.onMessage;
  }

  onClickLoad = () => {
  };

  onClickClear = () => {
    this.refs.console.setState({
      history: [],
      acceptInput: true,
      historyn: 0,
      kill: [],
      log: [],
    });
  };

  onClickRun = () => {
    this.run();
  };

  componentDidUpdate() {
    this.refs.buildlogs.scrollTop = this.refs.buildlogs.offsetHeight;
  }

  render() {
    const layout = [
      {
        i: 'editor',
        x: 0,
        y: 0,
        w: 6,
        minW: 3,
        maxW: 12,
        h: 10,
      },
      {
        i: 'repl',
        x: 6,
        y: 0,
        w: 6,
        minW: 3,
        maxW: 12,
        h: 10,
      },
      {
        i: 'controls',
        x: 0,
        y: 20,
        w: 12,
        minW: 3,
        maxW: 12,
        h: 1.2,
      },
      {
        i: 'buildlogs',
        x: 0,
        y: 12,
        w: 12,
        minW: 3,
        maxW: 12,
        h: 5,
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

        <div key="buildlogs" className="BuildLogs">
          {this.state && this.state.logs.map((l, i) => {
            return (
              <div key={i} className={l.className}>{l.text}</div>
            );
          })}
        </div>

        <div
          key="editor"
          onKeyDown={(e) => {
            if (e.ctrlKey && e.key === 'r') {
              this.run();
            } else if (e.ctrlKey && e.key === 'l') {
              this.load();
            }
          }}
        >
          <Codemirror
            value={this.state && this.state.value}
            onChange={(t) => this.setState({ value: t })}
            options={{
              theme: '3024-night',
              mode: 'haskell',
              lineNumbers: true,
            }}
          />
        </div>

        <div
          key="repl"
          onKeyDown={(e) => {
            if (e.ctrlKey && e.key === 'l') {
              this.onClickClear();
            }
          }}
        >
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
