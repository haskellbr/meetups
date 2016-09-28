const DataLoader = require('dataloader');
const Promise = require('bluebird');
const fetch = require('isomorphic-fetch');
const keyBy = require('lodash/keyBy');
const sqlite3 = require('sqlite3');

const db = new sqlite3.Database('./chinook.db');

function loggedQuery(query) {
  console.log(query);
  return new Promise((resolve, reject) => {
    db.all(query, (err, results) => {
      if (err) reject(err);
      else resolve(results);
    });
  });
}

const invoiceLoader = new DataLoader((keys) => {
  return loggedQuery(
    'SELECT * FROM Invoices WHERE InvoiceId IN (' + keys.join(',') + ')'
  ).then((results) => {
    const indexedResults = keyBy(results || [], 'InvoiceId')
    return keys.map((key) => {
      return indexedResults[key];
    });
  });
});

const customerLoader = new DataLoader((keys) => {
  return loggedQuery(
    'SELECT * FROM Customers WHERE CustomerId IN (' + keys.join(',') + ')'
  ).then((results) => {
    const indexedResults = keyBy(results || [], 'CustomerId')
    return keys.map((key) => {
      return indexedResults[key];
    });
  });
});

const p1 = invoiceLoader.load(11);
const p2 = invoiceLoader.load(9);
const p3 = invoiceLoader.load(5);
const p4 = invoiceLoader.load(222);

Promise.join(p1, p2, p3, p4).spread((r1, r2, r3, r4) => {
  console.log('  i1 ->', JSON.stringify(r1));
  console.log('  i2 ->', JSON.stringify(r2));
  console.log('  i3 ->', JSON.stringify(r3));
  console.log('  i4 ->', JSON.stringify(r4));

  return Promise.map([r1, r2, r3, r4], (r) => {
    return customerLoader.load(r.CustomerId);
  }).spread((c1, c2, c3, c4) => {
    console.log('  c1 ->', JSON.stringify(c1));
    console.log('  c2 ->', JSON.stringify(c2));
    console.log('  c3 ->', JSON.stringify(c3));
    console.log('  c4 ->', JSON.stringify(c4));
  });
});
