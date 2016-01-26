var browserify = require('browserify-middleware');
var child_process = require('child_process');
var express = require('express');
var byline = require('byline');
var http = require('http');
var socketio = require('socket.io');

var app = express();

app.use('/js', browserify(__dirname + '/client'));
app.use('/static', express.static('./'));
app.get('/', function(req, res) {
  res.sendfile('index.html');
});

app.get('/delay', function(req, res) {
  console.log(req.query.timeout);
  setTimeout(function() {
    res.end('ok');
  }, req.query.timeout || (1000 + 10000 * Math.random()));
});

var server = http.Server(app);

var io = socketio(server);
io.on('connection', function(socket) {
  console.log('User connected; spawning GHCi instance');

  var ghci = child_process.spawn('stack', ['ghci']);
  ghci.stdin.write(':set prompt >\n');
  ghci.stdin.write(':l ConcurrentWatched.hs\n');
  //ghci.stdin.write('import System.IO\n');
  //ghci.stdin.write('hSetBuffering stdout LineBuffering\n');
  var ghciStdoutByLine = byline(ghci.stdout);
  var ghciStderrByLine = byline(ghci.stderr);
  var nextEvent = 'echo';

  function handleGhciData(name, data) {
    data = data.toString();
    if(data === '>') return;
    data = data.replace(/^(> ?)+/g, '');
    console.log('Writting ' + name, data, 'to socket');
    socket.emit(name === 'stderr' ? 'gError' : nextEvent, data);
    nextEvent = 'echo';
  }

  ghciStdoutByLine.on('data', handleGhciData.bind(null, 'stdout'));
  ghciStderrByLine.on('data', handleGhciData.bind(null, 'stderr'));

  socket.on('command', function(d) {
    console.log('Writting', d, 'to GHCi');
    nextEvent = 'echo';
    ghci.stdin.write(d + '\n');
  });

  socket.on('readMVar', function(d) {
    d = 'tryReadMVar ' + d;
    console.log('Writting', d, 'to GHCi');
    nextEvent = 'readMVar';
    ghci.stdin.write(d + '\n');
  });

  socket.on('threadStatesJSON', function() {
    var d = 'threadStatesJSON';
    console.log('Writting', d, 'to GHCi');
    nextEvent = 'threadStatesJSON';
    ghci.stdin.write(d + '\n');
  });

  socket.on('disconnect', function() {
    console.log('User disconnected, killing GHCi');
    ghci.kill();
  });
});

server.listen(4000, function() {
  console.log('listening on *:3000');
});
