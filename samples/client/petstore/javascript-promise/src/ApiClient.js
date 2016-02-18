(function(root, factory) {
  if (typeof define === 'function' && define.amd) {
    // AMD. Register as an anonymous module.
    define(['superagent'], factory);
  } else if (typeof module === 'object' && module.exports) {
    // CommonJS-like environments that support module.exports, like Node.
    module.exports = factory(require('superagent'));
  } else {
    // Browser globals (root is window)
    if (!root.SwaggerPetstore) {
      root.SwaggerPetstore = {};
    }
    root.SwaggerPetstore.ApiClient = factory(root.superagent);
  }
}(this, function(superagent) {
  'use strict';

  var ApiClient = function ApiClient() {
    /**
     * The base path to put in front of every API call's (relative) path.
     */
    this.basePath = 'http://petstore.swagger.io/v2'.replace(/\/+$/, '');

    this.authentications = {
      'petstore_auth': {type: 'oauth2'},
      'test_api_client_id': {type: 'apiKey', in: 'header', name: 'x-test_api_client_id'},
      'test_api_client_secret': {type: 'apiKey', in: 'header', name: 'x-test_api_client_secret'},
      'api_key': {type: 'apiKey', in: 'header', name: 'api_key'},
      'test_api_key_query': {type: 'apiKey', in: 'query', name: 'test_api_key_query'},
      'test_api_key_header': {type: 'apiKey', in: 'header', name: 'test_api_key_header'}
    };

    /**
     * The default HTTP headers to be included for all API calls.
     */
    this.defaultHeaders = {};
  };

  ApiClient.prototype.paramToString = function paramToString(param) {
    if (param == null) {
      // return empty string for null and undefined
      return '';
    } else if (param instanceof Date) {
      return param.toJSON();
    } else {
      return param.toString();
    }
  };

  /**
   * Build full URL by appending the given path to base path and replacing
   * path parameter placeholders with parameter values.
   * NOTE: query parameters are not handled here.
   */
  ApiClient.prototype.buildUrl = function buildUrl(path, pathParams) {
    if (!path.match(/^\//)) {
      path = '/' + path;
    }
    var url = this.basePath + path;
    var _this = this;
    url = url.replace(/\{([\w-]+)\}/g, function(fullMatch, key) {
      var value;
      if (pathParams.hasOwnProperty(key)) {
        value = _this.paramToString(pathParams[key]);
      } else {
        value = fullMatch;
      }
      return encodeURIComponent(value);
    });
    return url;
  };

  /**
   * Check if the given MIME is a JSON MIME.
   * JSON MIME examples:
   *   application/json
   *   application/json; charset=UTF8
   *   APPLICATION/JSON
   */
  ApiClient.prototype.isJsonMime = function isJsonMime(mime) {
    return Boolean(mime != null && mime.match(/^application\/json(;.*)?$/i));
  };

  /**
   * Choose a MIME from the given MIMEs with JSON preferred,
   * i.e. return JSON if included, otherwise return the first one.
   */
  ApiClient.prototype.jsonPreferredMime = function jsonPreferredMime(mimes) {
    var len = mimes.length;
    for (var i = 0; i < len; i++) {
      if (this.isJsonMime(mimes[i])) {
        return mimes[i];
      }
    }
    return mimes[0];
  };

  /**
   * Check if the given parameter value is like file content.
   */
  ApiClient.prototype.isFileParam = function isFileParam(param) {
    // fs.ReadStream in Node.js (but not in runtime like browserify)
    if (typeof window === 'undefined' &&
        typeof require === 'function' &&
        require('fs') &&
        param instanceof require('fs').ReadStream) {
      return true;
    }
    // Buffer in Node.js
    if (typeof Buffer === 'function' && param instanceof Buffer) {
      return true;
    }
    // Blob in browser
    if (typeof Blob === 'function' && param instanceof Blob) {
      return true;
    }
    // File in browser (it seems File object is also instance of Blob, but keep this for safe)
    if (typeof File === 'function' && param instanceof File) {
      return true;
    }
    return false;
  };

  /**
   * Normalize parameters values:
   *   remove nils,
   *   keep files and arrays,
   *   format to string with `paramToString` for other cases.
   */
  ApiClient.prototype.normalizeParams = function normalizeParams(params) {
    var newParams = {};
    for (var key in params) {
      if (params.hasOwnProperty(key) && params[key] != null) {
        var value = params[key];
        if (this.isFileParam(value) || Array.isArray(value)) {
          newParams[key] = value;
        } else {
          newParams[key] = this.paramToString(value);
        }
      }
    }
    return newParams;
  };

  /**
   * Build parameter value according to the given collection format.
   * @param {String} collectionFormat one of 'csv', 'ssv', 'tsv', 'pipes' and 'multi'
   */
  ApiClient.prototype.buildCollectionParam = function buildCollectionParam(param, collectionFormat) {
    if (param == null) {
      return null;
    }
    switch (collectionFormat) {
      case 'csv':
        return param.map(this.paramToString).join(',');
      case 'ssv':
        return param.map(this.paramToString).join(' ');
      case 'tsv':
        return param.map(this.paramToString).join('\t');
      case 'pipes':
        return param.map(this.paramToString).join('|');
      case 'multi':
        // return the array directly as Superagent will handle it as expected
        return param.map(this.paramToString);
      default:
        throw new Error('Unknown collection format: ' + collectionFormat);
    }
  };

  ApiClient.prototype.applyAuthToRequest = function applyAuthToRequest(request, authNames) {
    var _this = this;
    authNames.forEach(function(authName) {
      var auth = _this.authentications[authName];
      switch (auth.type) {
        case 'basic':
          if (auth.username || auth.password) {
            request.auth(auth.username || '', auth.password || '');
          }
          break;
        case 'apiKey':
          if (auth.apiKey) {
            var data = {};
            if (auth.apiKeyPrefix) {
              data[auth.name] = auth.apiKeyPrefix + ' ' + auth.apiKey;
            } else {
              data[auth.name] = auth.apiKey;
            }
            if (auth.in === 'header') {
              request.set(data);
            } else {
              request.query(data);
            }
          }
          break;
        case 'oauth2':
          if (auth.accessToken) {
            request.set({'Authorization': 'Bearer ' + auth.accessToken});
          }
          break;
        default:
          throw new Error('Unknown authentication type: ' + auth.type);
      }
    });
  };

  ApiClient.prototype.deserialize = function deserialize(response, returnType) {
    if (response == null || returnType == null) {
      return null;
    }
    // Rely on Superagent for parsing response body.
    // See http://visionmedia.github.io/superagent/#parsing-response-bodies
    var data = response.body;
    if (data == null) {
      // Superagent does not always produce a body; use the unparsed response
      // as a fallback
      data = response.text;
    }
    return ApiClient.convertToType(data, returnType);
  };

  ApiClient.prototype.callApi = function callApi(path, httpMethod, pathParams,
      queryParams, headerParams, formParams, bodyParam, authNames, contentTypes,
      accepts, returnType) {
    var _this = this;
    var url = this.buildUrl(path, pathParams);
    var request = superagent(httpMethod, url);

    // apply authentications
    this.applyAuthToRequest(request, authNames);

    // set query parameters
    request.query(this.normalizeParams(queryParams));

    // set header parameters
    request.set(this.defaultHeaders).set(this.normalizeParams(headerParams));

    var contentType = this.jsonPreferredMime(contentTypes);
    if (contentType) {
      request.type(contentType);
    } else if (!request.header['Content-Type']) {
      request.type('application/json');
    }

    if (contentType === 'application/x-www-form-urlencoded') {
      request.send(this.normalizeParams(formParams));
    } else if (contentType == 'multipart/form-data') {
      var _formParams = this.normalizeParams(formParams);
      for (var key in _formParams) {
        if (_formParams.hasOwnProperty(key)) {
          if (this.isFileParam(_formParams[key])) {
            // file field
            request.attach(key, _formParams[key]);
          } else {
            request.field(key, _formParams[key]);
          }
        }
      }
    } else if (bodyParam) {
      request.send(bodyParam);
    }

    var accept = this.jsonPreferredMime(accepts);
    if (accept) {
      request.accept(accept);
    }

    
    return new Promise( function(resolve,reject) {
      request.end(function(error, response) {
        if (error) {
          reject(error);
        }
        else {
          var data = _this.deserialize(response, returnType);
          resolve(data);
        }
      });
    });
    
    
  };

  ApiClient.parseDate = function parseDate(str) {
    str = str.replace(/T/i, ' ');
    return new Date(str);
  };

  ApiClient.convertToType = function convertToType(data, type) {
    switch (type) {
      case 'Boolean':
        return Boolean(data);
      case 'Integer':
        return parseInt(data, 10);
      case 'Number':
        return parseFloat(data);
      case 'String':
        return String(data);
      case 'Date':
        return this.parseDate(String(data));
      default:
        if (typeof type === 'function') {
          // for model type like: User
          var model = type.constructFromObject(data);
          return model;
        } else if (Array.isArray(type)) {
          // for array type like: ['String']
          var itemType = type[0];
          return data.map(function(item) {
            return ApiClient.convertToType(item, itemType);
          });
        } else if (typeof type === 'object') {
          // for plain object type like: {'String': 'Integer'}
          var keyType, valueType;
          for (var k in type) {
            if (type.hasOwnProperty(k)) {
              keyType = k;
              valueType = type[k];
              break;
            }
          }
          var result = {};
          for (var k in data) {
            if (data.hasOwnProperty(k)) {
              var key = ApiClient.convertToType(k, keyType);
              var value = ApiClient.convertToType(data[k], valueType);
              result[key] = value;
            }
          }
          return result;
        } else {
          // for unknown type, return the data directly
          return data;
        }
    }
  };

  ApiClient.default = new ApiClient();

  return ApiClient;
}));
