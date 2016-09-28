const DataLoader = require('dataloader');
const Promise = require('bluebird');
const fetch = require('isomorphic-fetch');

function loggedFetch(url, options) {
  console.log('fetching', url, '...');
  return fetch(url, options).then(res => {
    return res.text()
  }).then(text => {
    console.log('fetched', url);
    return text;
  });
}

const loader = new DataLoader((urls) => {
  const seenUrls = {};
  return Promise.map(urls, (url) => {
    return loggedFetch(url);
  });
});

const p1 = loader.load('https://stackoverflow.com');
const p2 = loader.load('https://stackoverflow.com');
const p3 = loader.load('https://stackoverflow.com');
const p4 = loader.load('https://beijaflor.io');

Promise.join(p1, p2, p3, p4).spread((r1, r2, r3, r4) => {
  console.log('  r1 ->', r1.length);
  console.log('  r2 ->', r2.length);
  console.log('  r3 ->', r3.length);
  console.log('  r4 ->', r4.length);
  setTimeout(() => {}, 100);
});
