var $ = require('jquery');
var Promise = require('bluebird');
window.jQuery = $;
require('jquery.terminal');

var socket = io();

setTimeout(function() {
  setInterval(function() {
    socket.emit('threadStatesJSON');
    socket.once('threadStatesJSON', function(msg) {
      var threads = JSON.parse(msg);
      $('.threads-drawer .threads ul').html(Object.keys(threads).map(function(threadId) {
        var threadState = threads[threadId];
        return '<li class="' + threadState + '">' + threadId + ' ===> <span class="state">' + threadState + '</span></li>';
      }));
    });
  }, 1000);
}, 10000);

var mvarsHash = {};

$('.clear-mvars').on('click', function() {
  mvarsHash = {};
  syncMVars();
  return false;
});

function syncMVar(name) {
  return new Promise(function(fulfill) {
    console.log('readMVar ' + name);
    socket.emit('readMVar', name);
    socket.once('readMVar', function(result) {
      fulfill(result);
    });
  });
}

function syncMVars() {
  var mvars = Object.keys(mvarsHash);
  Promise.mapSeries(mvars, function(name) {
    return syncMVar(name);
  }).then(function(results) {
    console.log('results =', results);
    $('.threads-drawer .mvars ul').html(
      results.map(function(result, i) {
        if(result === 'Nothing') {
          return '<li class="empty">tryReadMVar ' + mvars[i] + ' ===> Nothing</li>';
        }

        return '<li>tryReadMVar' + mvars[i] + ' ===> ' + result + '</li>';
      }).join('\n')
    );
  });
}

var term = $('.terminal').terminal(function(command) {
  var m = /(\w+) <- (newEmptyMVar|newMVar)/.exec(command);
  if(m) {
    var name = m[1];
    mvarsHash[name] = true;
  }

  setTimeout(function() {
    syncMVars();
  }, 500);

  socket.emit('command', command);
}, {
  greetings: 'Web GHCi - Alpha Version',
  name: 'web_ghci',
  height: '100%',
  prompt: 'ghci> ',
});

socket.on('echo', function(data) {
  if(data === '{}') return;
  if(data.indexOf('{"ThreadId') === 0) return;
  term.echo(data);
});

socket.on('error', function(data) {
  term.error(data);
});
