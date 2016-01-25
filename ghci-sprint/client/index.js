var $ = require('jquery');
var Promise = require('bluebird');
window.jQuery = $;
require('jquery.terminal');

var socket = io();
var variablesHash = {};

$('.clear-variables').on('click', function() {
  variablesHash = {};
  syncVariables();
  return false;
});

$('.activate-form-annotations').on('change', function() {
  $('.sprint-drawer').toggleClass('annotations', $(this).prop('checked'));
});

function syncVariable(name) {
  return new Promise(function(fulfill) {
    console.log(':sprint ' + name);
    socket.emit('sprint', name);
    socket.once('sprint', function(result) {
      fulfill(result);
    });
  });
}

function syncVariables() {
  var variables = Object.keys(variablesHash);
  console.log('Syncing variables', variables);
  return Promise.mapSeries(variables, function(variable) {
    return syncVariable(variable);
  }).then(function(results) {
    console.log('results =', results);
    $('.sprint-drawer ul').html(
      results.map(function(result, i) {
        if(result.indexOf(variables[i]) === -1) {
          result = variables[i] + ' = ' + result;
        }

        var form = /= _$/.test(result) ? '' : (result.indexOf('_') === -1 ? 'nf' : 'whnf');
        var humanForm = '';
        if(form === 'nf') humanForm = 'Normal Form';
        else if(form === 'whnf') humanForm = 'Weak Head Normal Form'

        return '<li class="' + form + '"><code>:sprint ' + variables[i] + ' ===> ' + result + '</code><span class="form">' + humanForm + '</span></li>';
      }).join('\n')
    );
  });
}

var term = $('.terminal').terminal(function(command) {
  if(command.indexOf('let ') === 0) {
    variablesHash[command.slice(4).split(' ')[0]] = true;
  }

  setTimeout(function() {
    syncVariables();
  }, 500);

  socket.emit('command', command);
}, {
  greetings: 'Web GHCi - Alpha Version',
  name: 'web_ghci',
  height: '100%',
  prompt: 'ghci> ',
});

socket.on('echo', function(data) {
  term.echo(data);
});

socket.on('gError', function(data) {
  term.error(data);
});
