'use strict';

var swaggerTools = require('swagger-tools');
var jsyaml = require('js-yaml');
var fs = require('fs');

// swaggerRouter configuration
var options = {
  controllers: './controllers',
  useStubs: false
};

// The Swagger document (require it, build it programmatically, fetch it from a URL, ...)
var spec = fs.readFileSync('./api/swagger.yaml', 'utf8');
var swaggerDoc = jsyaml.safeLoad(spec);

function toPromise(f, req, res) {
  return new Promise(function(resolve, reject) {
    f(req, res, function(err) {
      if (err) {
        reject(err);
      } else {
        resolve();
      }
    });
  });
}

exports.v2 = function(req, res) {
  swaggerTools.initializeMiddleware(swaggerDoc, function(middleware) {
    var metadata = middleware.swaggerMetadata();
    var validator = middleware.swaggerValidator();
    var router = middleware.swaggerRouter(options);
    req.url = swaggerDoc.basePath + req.url;
    toPromise(metadata, req, res).then(function() {
      return toPromise(validator, req, res);
    }).then(function() {
      return toPromise(router, req, res);
    }).catch(function(err) {
      console.error(err);
      res.status(res.statusCode || 400).send(err);
    });
  });
};
