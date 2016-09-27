const DataLoader = require('dataloader');
const Promise = require('bluebird');
const fetch = require('isomorphic-fetch');
const keyBy = require('lodash/keyBy');
const sqlite3 = require('sqlite3');

const db = new sqlite3.Database('./chinook.db');

function loggedQuery(query, params) {
  console.log(query, params);
  return new Promise((resolve, reject) => {
    db.all(query, params, (err, results) => {
      if (err) reject(err);
      else resolve(results);
    });
  });
}

const loader = new DataLoader((keys) => {
  return loggedQuery(
    'SELECT * FROM Invoices WHERE InvoiceId IN (' + keys.join(',') + ')'
  ).then((results) => {
    const indexedResults = keyBy(results || [], 'InvoiceId')
    return keys.map((key) => {
      return indexedResults[key];
    });
  });
});

const p1 = loader.load(11);
const p2 = loader.load(9);
const p3 = loader.load(5);
const p4 = loader.load(222);

Promise.join(p1, p2, p3, p4).spread((r1, r2, r3, r4) => {
  console.log('  r1 ->', JSON.stringify(r1));
  console.log('  r2 ->', JSON.stringify(r2));
  console.log('  r3 ->', JSON.stringify(r3));
  console.log('  r4 ->', JSON.stringify(r4));
  setTimeout(() => {}, 100);
});
