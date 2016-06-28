
/**
 * Module dependencies.
 */

var debug = require('debug')('superagent');
var formidable = require('formidable');
var FormData = require('form-data');
var Response = require('./response');
var parse = require('url').parse;
var format = require('url').format;
var resolve = require('url').resolve;
var methods = require('methods');
var Stream = require('stream');
var utils = require('./utils');
var extend = require('extend');
var Part = require('./part');
var mime = require('mime');
var https = require('https');
var http = require('http');
var fs = require('fs');
var qs = require('qs');
var zlib = require('zlib');
var util = require('util');
var pkg = require('../../package.json');

/**
 * Expose the request function.
 */

exports = module.exports = request;

/**
 * Expose the agent function
 */

exports.agent = require('./agent');

/**
 * Expose `Part`.
 */

exports.Part = Part;

/**
 * Noop.
 */

function noop(){};

/**
 * Expose `Response`.
 */

exports.Response = Response;

/**
 * Define "form" mime type.
 */

mime.define({
  'application/x-www-form-urlencoded': ['form', 'urlencoded', 'form-data']
});

/**
 * Protocol map.
 */

exports.protocols = {
  'http:': http,
  'https:': https
};

/**
 * Check if `obj` is an object.
 *
 * @param {Object} obj
 * @return {Boolean}
 * @api private
 */

function isObject(obj) {
  return null != obj && 'object' == typeof obj;
}

/**
 * Default serialization map.
 *
 *     superagent.serialize['application/xml'] = function(obj){
 *       return 'generated xml here';
 *     };
 *
 */

exports.serialize = {
  'application/x-www-form-urlencoded': qs.stringify,
  'application/json': JSON.stringify
};

/**
 * Default parsers.
 *
 *     superagent.parse['application/xml'] = function(res, fn){
 *       fn(null, res);
 *     };
 *
 */

exports.parse = require('./parsers');

/**
 * Initialize a new `Request` with the given `method` and `url`.
 *
 * @param {String} method
 * @param {String|Object} url
 * @api public
 */

function Request(method, url) {
  Stream.call(this);
  var self = this;
  if ('string' != typeof url) url = format(url);
  this._agent = false;
  this._formData = null;
  this.method = method;
  this.url = url;
  this.header = {
    'User-Agent': 'node-superagent/' + pkg.version
  };
  this.writable = true;
  this._redirects = 0;
  this.redirects(5);
  this.cookies = '';
  this.qs = {};
  this.qsRaw = [];
  this._redirectList = [];
  this._streamRequest = false;
  this.on('end', this.clearTimeout.bind(this));
}

/**
 * Inherit from `Stream`.
 */

util.inherits(Request, Stream);

/**
 * Write the field `name` and `val` for "multipart/form-data"
 * request bodies.
 *
 * ``` js
 * request.post('http://localhost/upload')
 *   .field('foo', 'bar')
 *   .end(callback);
 * ```
 *
 * @param {String} name
 * @param {String|Buffer|fs.ReadStream} val
 * @return {Request} for chaining
 * @api public
 */

Request.prototype.field = function(name, val){
  debug('field', name, val);
  if (!this._formData) this._formData = new FormData();
  this._formData.append(name, val);
  return this;
};

/**
 * Queue the given `file` as an attachment to the specified `field`,
 * with optional `filename`.
 *
 * ``` js
 * request.post('http://localhost/upload')
 *   .attach(new Buffer('<b>Hello world</b>'), 'hello.html')
 *   .end(callback);
 * ```
 *
 * A filename may also be used:
 *
 * ``` js
 * request.post('http://localhost/upload')
 *   .attach('files', 'image.jpg')
 *   .end(callback);
 * ```
 *
 * @param {String} field
 * @param {String|fs.ReadStream|Buffer} file
 * @param {String} filename
 * @return {Request} for chaining
 * @api public
 */

Request.prototype.attach = function(field, file, filename){
  if (!this._formData) this._formData = new FormData();
  if ('string' == typeof file) {
    if (!filename) filename = file;
    debug('creating `fs.ReadStream` instance for file: %s', file);
    file = fs.createReadStream(file);
  } else if(!filename && file.path) {
    filename = file.path;
  }
  this._formData.append(field, file, { filename: filename });
  return this;
};

/**
 * Set the max redirects to `n`.
 *
 * @param {Number} n
 * @return {Request} for chaining
 * @api public
 */

Request.prototype.redirects = function(n){
  debug('max redirects %s', n);
  this._maxRedirects = n;
  return this;
};

/**
 * Return a new `Part` for this request.
 *
 * @return {Part}
 * @api public
 * @deprecated pass a readable stream in to `Request#attach()` instead
 */

Request.prototype.part = util.deprecate(function(){
  return new Part(this);
}, '`Request#part()` is deprecated. ' +
   'Pass a readable stream in to `Request#attach()` instead.');

/**
 * Gets/sets the `Agent` to use for this HTTP request. The default (if this
 * function is not called) is to opt out of connection pooling (`agent: false`).
 *
 * @param {http.Agent} agent
 * @return {http.Agent}
 * @api public
 */

Request.prototype.agent = function(agent){
  if (!arguments.length) return this._agent;
  this._agent = agent;
  return this;
};

/**
 * Set header `field` to `val`, or multiple fields with one object.
 *
 * Examples:
 *
 *      req.get('/')
 *        .set('Accept', 'application/json')
 *        .set('X-API-Key', 'foobar')
 *        .end(callback);
 *
 *      req.get('/')
 *        .set({ Accept: 'application/json', 'X-API-Key': 'foobar' })
 *        .end(callback);
 *
 * @param {String|Object} field
 * @param {String} val
 * @return {Request} for chaining
 * @api public
 */

Request.prototype.set = function(field, val){
  if (isObject(field)) {
    for (var key in field) {
      this.set(key, field[key]);
    }
    return this;
  }

  debug('set %s "%s"', field, val);
  this.header[field] = val;
  return this;
};

/**
 * Remove header `field`.
 *
 * Example:
 *
 *      req.get('/')
 *        .unset('User-Agent')
 *        .end(callback);
 *
 * @param {String} field
 * @return {Request} for chaining
 * @api public
 */

Request.prototype.unset = function(field){
  debug('unset %s', field);

  delete this.header[field];
  return this;
};

/**
 * Get request header `field`.
 *
 * @param {String} field
 * @return {String}
 * @api public
 */

Request.prototype.get = function(field){
  return this.header[field];
};

/**
 * Set _Content-Type_ response header passed through `mime.lookup()`.
 *
 * Examples:
 *
 *      request.post('/')
 *        .type('xml')
 *        .send(xmlstring)
 *        .end(callback);
 *
 *      request.post('/')
 *        .type('json')
 *        .send(jsonstring)
 *        .end(callback);
 *
 *      request.post('/')
 *        .type('application/json')
 *        .send(jsonstring)
 *        .end(callback);
 *
 * @param {String} type
 * @return {Request} for chaining
 * @api public
 */

Request.prototype.type = function(type){
  return this.set('Content-Type', ~type.indexOf('/')
    ? type
    : mime.lookup(type));
};

/**
 * Set _Accept_ response header passed through `mime.lookup()`.
 *
 * Examples:
 *
 *      superagent.types.json = 'application/json';
 *
 *      request.get('/agent')
 *        .accept('json')
 *        .end(callback);
 *
 *      request.get('/agent')
 *        .accept('application/json')
 *        .end(callback);
 *
 * @param {String} accept
 * @return {Request} for chaining
 * @api public
 */

Request.prototype.accept = function(type){
  return this.set('Accept', ~type.indexOf('/')
    ? type
    : mime.lookup(type));
};

/**
 * Add query-string `val`.
 *
 * Examples:
 *
 *   request.get('/shoes')
 *     .query('size=10')
 *     .query({ color: 'blue' })
 *
 * @param {Object|String} val
 * @return {Request} for chaining
 * @api public
 */

Request.prototype.query = function(val){
  if ('string' == typeof val) {
    this.qsRaw.push(val);
    return this;
  }

  extend(this.qs, val);
  return this;
};

/**
 * Send `data` as the request body, defaulting the `.type()` to "json" when
 * an object is given.
 *
 * Examples:
 *
 *       // manual json
 *       request.post('/user')
 *         .type('json')
 *         .send('{"name":"tj"}')
 *         .end(callback)
 *
 *       // auto json
 *       request.post('/user')
 *         .send({ name: 'tj' })
 *         .end(callback)
 *
 *       // manual x-www-form-urlencoded
 *       request.post('/user')
 *         .type('form')
 *         .send('name=tj')
 *         .end(callback)
 *
 *       // auto x-www-form-urlencoded
 *       request.post('/user')
 *         .type('form')
 *         .send({ name: 'tj' })
 *         .end(callback)
 *
 *       // string defaults to x-www-form-urlencoded
 *       request.post('/user')
 *         .send('name=tj')
 *         .send('foo=bar')
 *         .send('bar=baz')
 *         .end(callback)
 *
 * @param {String|Object} data
 * @return {Request} for chaining
 * @api public
 */

Request.prototype.send = function(data){
  var obj = isObject(data);
  var type = this.get('Content-Type');
  // merge
  if (obj && isObject(this._data)) {
    for (var key in data) {
      this._data[key] = data[key];
    }
  // string
  } else if ('string' == typeof data) {
    // default to x-www-form-urlencoded
    if (!type) this.type('form');
    type = this.get('Content-Type');

    // concat &
    if ('application/x-www-form-urlencoded' == type) {
      this._data = this._data
        ? this._data + '&' + data
        : data;
    } else {
      this._data = (this._data || '') + data;
    }
  } else {
    this._data = data;
  }

  if (!obj) return this;

  // default to json
  if (!type) this.type('json');
  return this;
};

/**
 * Write raw `data` / `encoding` to the socket.
 *
 * @param {Buffer|String} data
 * @param {String} encoding
 * @return {Boolean}
 * @api public
 */

Request.prototype.write = function(data, encoding){
  var req = this.request();
  if (!this._streamRequest) {
    this._streamRequest = true;
    try {
      // ensure querystring is appended before headers are sent
      this.appendQueryString(req);
    } catch (e) {
      return this.emit('error', e);
    }
  }
  return req.write(data, encoding);
};

/**
 * Pipe the request body to `stream`.
 *
 * @param {Stream} stream
 * @param {Object} options
 * @return {Stream}
 * @api public
 */

Request.prototype.pipe = function(stream, options){
  this.piped = true; // HACK...
  this.buffer(false);
  var self = this;
  this.end().req.on('response', function(res){
    // redirect
    var redirect = isRedirect(res.statusCode);
    if (redirect && self._redirects++ != self._maxRedirects) {
      return self.redirect(res).pipe(stream, options);
    }

    if (/^(deflate|gzip)$/.test(res.headers['content-encoding'])) {
      res.pipe(zlib.createUnzip()).pipe(stream, options);
    } else {
      res.pipe(stream, options);
    }
    res.on('end', function(){
      self.emit('end');
    });
  });
  return stream;
};

/**
 * Enable / disable buffering.
 *
 * @return {Boolean} [val]
 * @return {Request} for chaining
 * @api public
 */

Request.prototype.buffer = function(val){
  this._buffer = false === val
    ? false
    : true;
  return this;
};

/**
 * Set timeout to `ms`.
 *
 * @param {Number} ms
 * @return {Request} for chaining
 * @api public
 */

Request.prototype.timeout = function(ms){
  this._timeout = ms;
  return this;
};

/**
 * Clear previous timeout.
 *
 * @return {Request} for chaining
 * @api public
 */

Request.prototype.clearTimeout = function(){
  debug('clear timeout %s %s', this.method, this.url);
  this._timeout = 0;
  clearTimeout(this._timer);
  return this;
};

/**
 * Abort and clear timeout.
 *
 * @api public
 */

Request.prototype.abort = function(){
  debug('abort %s %s', this.method, this.url);
  this._aborted = true;
  this.clearTimeout();
  this.req.abort();
  this.emit('abort');
};

/**
 * Define the parser to be used for this response.
 *
 * @param {Function} fn
 * @return {Request} for chaining
 * @api public
 */

Request.prototype.parse = function(fn){
  this._parser = fn;
  return this;
};

/**
 * Redirect to `url
 *
 * @param {IncomingMessage} res
 * @return {Request} for chaining
 * @api private
 */

Request.prototype.redirect = function(res){
  var url = res.headers.location;
  if (!url) {
    return this.callback(new Error('No location header for redirect'), res);
  }

  debug('redirect %s -> %s', this.url, url);

  // location
  url = resolve(this.url, url);

  // ensure the response is being consumed
  // this is required for Node v0.10+
  res.resume();

  var headers = this.req._headers;

  var shouldStripCookie = parse(url).host !== parse(this.url).host;

  // implementation of 302 following defacto standard
  if (res.statusCode == 301 || res.statusCode == 302){
    // strip Content-* related fields
    // in case of POST etc
    headers = utils.cleanHeader(this.req._headers, shouldStripCookie);

    // force GET
    this.method = 'HEAD' == this.method
      ? 'HEAD'
      : 'GET';

    // clear data
    this._data = null;
  }
  // 303 is always GET
  if (res.statusCode == 303) {
    // strip Content-* related fields
    // in case of POST etc
    headers = utils.cleanHeader(this.req._headers, shouldStripCookie);

    // force method
    this.method = 'GET';

    // clear data
    this._data = null;
  }
  // 307 preserves method
  // 308 preserves method
  delete headers.host;

  delete this.req;
  delete this._formData;
  
  // remove all add header except User-Agent
  for (var key in this.header) {
    if (key !== 'User-Agent') {
      delete this.header[key]
    }
  }

  // redirect
  this.url = url;
  this._redirectList.push(url);
  this.emit('redirect', res);
  this.qs = {};
  this.qsRaw = [];
  this.set(headers);
  this.end(this._callback);
  return this;
};

/**
 * Set Authorization field value with `user` and `pass`.
 *
 * Examples:
 *
 *   .auth('tobi', 'learnboost')
 *   .auth('tobi:learnboost')
 *   .auth('tobi')
 *
 * @param {String} user
 * @param {String} pass
 * @return {Request} for chaining
 * @api public
 */

Request.prototype.auth = function(user, pass){
  if (1 === arguments.length) pass = '';
  if (!~user.indexOf(':')) user = user + ':';
  var str = new Buffer(user + pass).toString('base64');
  return this.set('Authorization', 'Basic ' + str);
};

/**
 * Set the certificate authority option for https request.
 *
 * @param {Buffer | Array} cert
 * @return {Request} for chaining
 * @api public
 */

Request.prototype.ca = function(cert){
  this._ca = cert;
  return this;
};

/**
 * Allow for extension
 */

Request.prototype.use = function(fn) {
  fn(this);
  return this;
};

/**
 * Return an http[s] request.
 *
 * @return {OutgoingMessage}
 * @api private
 */

Request.prototype.request = function(){
  if (this.req) return this.req;

  var self = this;
  var options = {};
  var data = this._data;
  var url = this.url;

  // default to http://
  if (0 != url.indexOf('http')) url = 'http://' + url;
  url = parse(url);

  // options
  options.method = this.method;
  options.port = url.port;
  options.path = url.pathname;
  options.host = url.hostname;
  options.ca = this._ca;
  options.agent = this._agent;

  // initiate request
  var mod = exports.protocols[url.protocol];

  // request
  var req = this.req = mod.request(options);
  if ('HEAD' != options.method) req.setHeader('Accept-Encoding', 'gzip, deflate');
  this.protocol = url.protocol;
  this.host = url.host;

  // expose events
  req.on('drain', function(){ self.emit('drain'); });

  req.on('error', function(err){
    // flag abortion here for out timeouts
    // because node will emit a faux-error "socket hang up"
    // when request is aborted before a connection is made
    if (self._aborted) return;
    // if we've recieved a response then we don't want to let
    // an error in the request blow up the response
    if (self.response) return;
    self.callback(err);
  });

  // auth
  if (url.auth) {
    var auth = url.auth.split(':');
    this.auth(auth[0], auth[1]);
  }

  // query
  if (url.search)
    this.query(url.search.substr(1));

  // add cookies
  if (this.cookies) req.setHeader('Cookie', this.cookies);

  for (var key in this.header) {
    req.setHeader(key, this.header[key]);
  }

  return req;
};

/**
 * Invoke the callback with `err` and `res`
 * and handle arity check.
 *
 * @param {Error} err
 * @param {Response} res
 * @api private
 */

Request.prototype.callback = function(err, res){
  // Avoid the error which is emitted from 'socket hang up' to cause the fn undefined error on JS runtime.
  var fn = this._callback || noop;
  this.clearTimeout();
  if (this.called) return console.warn('double callback!');
  this.called = true;

  if (err) {
    err.response = res;
  }

  // only emit error event if there is a listener
  // otherwise we assume the callback to `.end()` will get the error
  if (err && this.listeners('error').length > 0) this.emit('error', err);

  if (err) {
    return fn(err, res);
  }

  if (res && res.status >= 200 && res.status < 300) {
    return fn(err, res);
  }

  var msg = 'Unsuccessful HTTP response';
  if (res) {
    msg = http.STATUS_CODES[res.status] || msg;
  }
  var new_err = new Error(msg);
  new_err.original = err;
  new_err.response = res;
  new_err.status = (res) ? res.status : undefined;

  fn(err || new_err, res);
};

/**
 * Compose querystring to append to req.path
 *
 * @return {String} querystring
 * @api private
 */

Request.prototype.appendQueryString = function(req){
  var querystring = qs.stringify(this.qs, { indices: false });
  querystring += ((querystring.length && this.qsRaw.length) ? '&' : '') + this.qsRaw.join('&');
  req.path += querystring.length
    ? (~req.path.indexOf('?') ? '&' : '?') + querystring
    : '';
};

/**
 * Initiate request, invoking callback `fn(err, res)`
 * with an instanceof `Response`.
 *
 * @param {Function} fn
 * @return {Request} for chaining
 * @api public
 */

/**
* Client API parity, irrelevant in a Node context.
*
* @api public
*/

Request.prototype.withCredentials = function(){
  return this;
};

Request.prototype.end = function(fn){
  var self = this;
  var data = this._data;
  var req = this.request();
  var buffer = this._buffer;
  var method = this.method;
  var timeout = this._timeout;
  debug('%s %s', this.method, this.url);

  // store callback
  this._callback = fn || noop;

  // querystring
  try {
    this.appendQueryString(req);
  } catch (e) {
    return this.callback(e);
  }

  // timeout
  if (timeout && !this._timer) {
    debug('timeout %sms %s %s', timeout, this.method, this.url);
    this._timer = setTimeout(function(){
      var err = new Error('timeout of ' + timeout + 'ms exceeded');
      err.timeout = timeout;
      err.code = 'ECONNABORTED';
      self.abort();
      self.callback(err);
    }, timeout);
  }

  // body
  if ('HEAD' != method && !req._headerSent) {
    // serialize stuff
    if ('string' != typeof data) {
      var contentType = req.getHeader('Content-Type')
      // Parse out just the content type from the header (ignore the charset)
      if (contentType) contentType = contentType.split(';')[0]
      var serialize = exports.serialize[contentType];
      if (!serialize && isJSON(contentType)) serialize = exports.serialize['application/json'];
      if (serialize) data = serialize(data);
    }

    // content-length
    if (data && !req.getHeader('Content-Length')) {
      req.setHeader('Content-Length', Buffer.isBuffer(data) ? data.length : Buffer.byteLength(data));
    }
  }

  // response
  req.on('response', function(res){
    debug('%s %s -> %s', self.method, self.url, res.statusCode);
    var max = self._maxRedirects;
    var mime = utils.type(res.headers['content-type'] || '') || 'text/plain';
    var len = res.headers['content-length'];
    var type = mime.split('/');
    var subtype = type[1];
    var type = type[0];
    var multipart = 'multipart' == type;
    var redirect = isRedirect(res.statusCode);
    var parser = self._parser;

    self.res = res;

    if ('HEAD' == self.method) {
      var response = new Response(self);
      self.response = response;
      response.redirects = self._redirectList;
      self.emit('response', response);
      self.callback(null, response);
      self.emit('end');
      return;
    }

    if (self.piped) {
      return;
    }

    // redirect
    if (redirect && self._redirects++ != max) {
      return self.redirect(res);
    }

    // zlib support
    if (/^(deflate|gzip)$/.test(res.headers['content-encoding'])) {
      utils.unzip(req, res);
    }

    // don't buffer multipart
    if (multipart) buffer = false;

    // TODO: make all parsers take callbacks
    if (!parser && multipart) {
      var form = new formidable.IncomingForm;

      form.parse(res, function(err, fields, files){
        if (err) return self.callback(err);
        var response = new Response(self);
        self.response = response;
        response.body = fields;
        response.files = files;
        response.redirects = self._redirectList;
        self.emit('end');
        self.callback(null, response);
      });
      return;
    }

    // check for images, one more special treatment
    if (!parser && isImage(mime)) {
      exports.parse.image(res, function(err, obj){
        if (err) return self.callback(err);
        var response = new Response(self);
        self.response = response;
        response.body = obj;
        response.redirects = self._redirectList;
        self.emit('end');
        self.callback(null, response);
      });
      return;
    }

    // by default only buffer text/*, json and messed up thing from hell
    if (null == buffer && isText(mime) || isJSON(mime)) buffer = true;

    // parser
    var parse = 'text' == type
      ? exports.parse.text
      : exports.parse[mime];

    // everyone wants their own white-labeled json
    if (!parse && isJSON(mime)) parse = exports.parse['application/json'];

    // buffered response
    if (buffer) parse = parse || exports.parse.text;

    // explicit parser
    if (parser) parse = parser;

    // parse
    if (parse) {
      try {
        parse(res, function(err, obj){
          if (err && !self._aborted) self.callback(err);
          res.body = obj;
        });
      } catch (err) {
        self.callback(err);
        return;
      }
    }

    // unbuffered
    if (!buffer) {
      debug('unbuffered %s %s', self.method, self.url);
      self.res = res;
      var response = new Response(self);
      self.response = response;
      response.redirects = self._redirectList;
      self.emit('response', response);
      self.callback(null, response);
      if (multipart) return // allow multipart to handle end event
      res.on('end', function(){
        debug('end %s %s', self.method, self.url);
        self.emit('end');
      })
      return;
    }

    // terminating events
    self.res = res;
    res.on('error', function(err){
      self.callback(err, null);
    });
    res.on('end', function(){
      debug('end %s %s', self.method, self.url);
      // TODO: unless buffering emit earlier to stream
      var response = new Response(self);
      self.response = response;
      response.redirects = self._redirectList;
      self.emit('response', response);
      self.callback(null, response);
      self.emit('end');
    });
  });

  this.emit('request', this);

  // if a FormData instance got created, then we send that as the request body
  var formData = this._formData;
  if (formData) {

    // set headers
    var headers = formData.getHeaders();
    for (var i in headers) {
      debug('setting FormData header: "%s: %s"', i, headers[i]);
      req.setHeader(i, headers[i]);
    }

    // attempt to get "Content-Length" header
    formData.getLength(function(err, length) {
      // TODO: Add chunked encoding when no length (if err)

      debug('got FormData Content-Length: %s', length);
      if ('number' == typeof length) {
        req.setHeader('Content-Length', length);
      }

      var getProgressMonitor = function () {
        var lengthComputable = true;
        var total = req.getHeader('Content-Length');
        var loaded = 0;

        var progress = new Stream.Transform();
        progress._transform = function (chunk, encoding, cb) {
          loaded += chunk.length;
          self.emit('progress', {
            direction: 'upload',
            lengthComputable: lengthComputable,
            loaded: loaded,
            total: total
          });
          cb(null, chunk);
        };
        return progress;
      };
      formData.pipe(getProgressMonitor()).pipe(req);
    });
  } else {
    req.end(data);
  }

  return this;
};

/**
 * Faux promise support
 *
 * @param {Function} fulfill
 * @param {Function} reject
 * @return {Request}
 */

Request.prototype.then = function (fulfill, reject) {
  return this.end(function(err, res) {
    err ? reject(err) : fulfill(res);
  });
}

/**
 * To json.
 *
 * @return {Object}
 * @api public
 */

Request.prototype.toJSON = function(){
  return {
    method: this.method,
    url: this.url,
    data: this._data
  };
};

/**
 * Expose `Request`.
 */

exports.Request = Request;

/**
 * Issue a request:
 *
 * Examples:
 *
 *    request('GET', '/users').end(callback)
 *    request('/users').end(callback)
 *    request('/users', callback)
 *
 * @param {String} method
 * @param {String|Function} url or callback
 * @return {Request}
 * @api public
 */

function request(method, url) {
  // callback
  if ('function' == typeof url) {
    return new Request('GET', method).end(url);
  }

  // url first
  if (1 == arguments.length) {
    return new Request('GET', method);
  }

  return new Request(method, url);
}

// generate HTTP verb methods
if (methods.indexOf('del') == -1) {
  // create a copy so we don't cause conflicts with
  // other packages using the methods package and
  // npm 3.x
  methods = methods.slice(0);
  methods.push('del');
}
methods.forEach(function(method){
  var name = method;
  method = 'del' == method ? 'delete' : method;

  method = method.toUpperCase();
  request[name] = function(url, data, fn){
    var req = request(method, url);
    if ('function' == typeof data) fn = data, data = null;
    if (data) req.send(data);
    fn && req.end(fn);
    return req;
  };
});

/**
 * Check if `mime` is text and should be buffered.
 *
 * @param {String} mime
 * @return {Boolean}
 * @api public
 */

function isText(mime) {
  var parts = mime.split('/');
  var type = parts[0];
  var subtype = parts[1];

  return 'text' == type
    || 'x-www-form-urlencoded' == subtype;
}

/**
 * Check if `mime` is image
 *
 * @param {String} mime
 * @return {Boolean}
 * @api public
 */

function isImage(mime) {
  var parts = mime.split('/');
  var type = parts[0];
  var subtype = parts[1];

  return 'image' == type;
}

/**
 * Check if `mime` is json or has +json structured syntax suffix.
 *
 * @param {String} mime
 * @return {Boolean}
 * @api private
 */

function isJSON(mime) {
  return /[\/+]json\b/.test(mime);
}

/**
 * Check if we should follow the redirect `code`.
 *
 * @param {Number} code
 * @return {Boolean}
 * @api private
 */

function isRedirect(code) {
  return ~[301, 302, 303, 305, 307, 308].indexOf(code);
}
