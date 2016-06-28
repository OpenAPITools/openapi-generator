# SuperAgent [![Build Status](https://travis-ci.org/visionmedia/superagent.svg?branch=master)](https://travis-ci.org/visionmedia/superagent)

[![Sauce Test Status](https://saucelabs.com/browser-matrix/shtylman-superagent.svg)](https://saucelabs.com/u/shtylman-superagent)

SuperAgent is a small progressive __client-side__ HTTP request library, and __Node.js__ module with the same API, sporting many high-level HTTP client features. View the [docs](http://visionmedia.github.io/superagent/).

![super agent](http://f.cl.ly/items/3d282n3A0h0Z0K2w0q2a/Screenshot.png)

## Installation

node:

```
$ npm install superagent
```

component:

```
$ component install visionmedia/superagent
```

Works with [browserify](https://github.com/substack/node-browserify) and should work with [webpack](https://github.com/visionmedia/superagent/wiki/Superagent-for-Webpack)

```js
request
  .post('/api/pet')
  .send({ name: 'Manny', species: 'cat' })
  .set('X-API-Key', 'foobar')
  .set('Accept', 'application/json')
  .end(function(err, res){
    // Calling the end function will send the request
  });
```

## Supported browsers

Tested browsers:

- Latest Android
- Latest Firefox
- Latest Chrome
- IE9 through latest
- Latest iPhone
- Latest Safari

Even though IE9 is supported, a polyfill `window.btoa` is needed to use basic auth.

# Plugins

Superagent is easily extended via plugins.

```js
var nocache = require('superagent-no-cache');
var request = require('superagent');
var prefix = require('superagent-prefix')('/static');

request
.get('/some-url')
.use(prefix) // Prefixes *only* this request
.use(nocache) // Prevents caching of *only* this request
.end(function(err, res){
    // Do something
});
```

Existing plugins:
 * [superagent-no-cache](https://github.com/johntron/superagent-no-cache) - prevents caching by including Cache-Control header
 * [superagent-prefix](https://github.com/johntron/superagent-prefix) - prefixes absolute URLs (useful in test environment)
 * [superagent-mock](https://github.com/M6Web/superagent-mock) - simulate HTTP calls by returning data fixtures based on the requested URL
 * [superagent-mocker](https://github.com/shuvalov-anton/superagent-mocker) — simulate REST API
 * [superagent-cache](https://github.com/jpodwys/superagent-cache) - superagent with built-in, flexible caching
 * [superagent-jsonapify](https://github.com/alex94puchades/superagent-jsonapify) - A lightweight [json-api](http://jsonapi.org/format/) client addon for superagent
 * [superagent-serializer](https://github.com/zzarcon/superagent-serializer) - Converts server payload into different cases
 
Please prefix your plugin with `superagent-*` so that it can easily be found by others.

For superagent extensions such as couchdb and oauth visit the [wiki](https://github.com/visionmedia/superagent/wiki).

## Running node tests

Install dependencies:

```shell
$ npm install
```
Run em!

```shell
$ make test
```

## Running browser tests

Install dependencies:

```shell
$ npm install
```

Start the test runner:

```shell
$ make test-browser-local
```

Visit `http://localhost:4000/__zuul` in your browser.

Edit tests and refresh your browser. You do not have to restart the test runner.

## License

MIT
