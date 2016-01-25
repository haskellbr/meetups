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

var server = http.Server(app);

var io = socketio(server);
io.on('connection', function(socket) {
  console.log('User connected; spawning GHCi instance');

  var ghci = child_process.spawn('ghci');
  ghci.stdin.write(':set prompt >\n');
  var ghciStdoutByLine = byline(ghci.stdout);
  var ghciStderrByLine = byline(ghci.stderr);
  var nextEvent = 'echo';

  function handleGhciData(name, data) {
    data = data.toString();
    if(data === '>') return;
    data = data.replace(/^(> ?)+/g, '');
    console.log('Writting ' + name, data, 'to socket');
    socket.emit(name !== 'stderr' ? nextEvent : 'gError', data);
  }

  ghciStdoutByLine.on('data', handleGhciData.bind(null, 'stdout'));
  ghciStderrByLine.on('data', handleGhciData.bind(null, 'stderr'));

  socket.on('command', function(d) {
    console.log('Writting', d, 'to GHCi');
    nextEvent = 'echo';
    ghci.stdin.write(d + '\n');
  });

  socket.on('sprint', function(d) {
    console.log('Handling internal sprint call', d);
    nextEvent = 'sprint';
    ghci.stdin.write(':sprint ' + d + '\n');
  });

  socket.on('disconnect', function() {
    console.log('User disconnected, killing GHCi');
    ghci.kill();
  });
});

server.listen(3000, function() {
  console.log('listening on *:3000');
});
