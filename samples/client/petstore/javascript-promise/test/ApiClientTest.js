if (typeof module === 'object' && module.exports) {
  var expect = require('expect.js');
  var SwaggerPetstore = require('../src/index');
  var sinon = require('sinon');
}

var apiClient = SwaggerPetstore.ApiClient.instance;

describe('ApiClient', function() {
  describe('defaults', function() {
    it('should have correct default values with the default API client', function() {
      expect(apiClient).to.be.ok();
      expect(apiClient.basePath).to.be('http://petstore.swagger.io:80/v2');
      expect(apiClient.authentications).to.eql({
        petstore_auth: {type: 'oauth2'},
        http_basic_test: {type: 'basic'},
        api_key: {type: 'apiKey', 'in': 'header', name: 'api_key'}
      /* comment out the following as these fake security def (testing purpose)
       * are removed from the spec, we'll add these back after updating the 
       * petstore server
       *
        test_http_basic: {type: 'basic'},
        test_api_client_id: {
          type: 'apiKey',
          'in': 'header',
          name: 'x-test_api_client_id'
        },
        test_api_client_secret: {
          type: 'apiKey',
          'in': 'header',
          name: 'x-test_api_client_secret'
        },
        test_api_key_query: {
          type: 'apiKey',
          'in': 'query',
          name: 'test_api_key_query'
        },
        test_api_key_header: {
          type: 'apiKey',
          'in': 'header',
          name: 'test_api_key_header'
        }*/
      });
    });

    it('should have correct default values with new API client and can customize it', function() {
      var newClient = new SwaggerPetstore.ApiClient;
      expect(newClient.basePath).to.be('http://petstore.swagger.io:80/v2');
      expect(newClient.buildUrl('/abc', {})).to.be('http://petstore.swagger.io:80/v2/abc');

      newClient.basePath = 'http://example.com';
      expect(newClient.basePath).to.be('http://example.com');
      expect(newClient.buildUrl('/abc', {})).to.be('http://example.com/abc');
    });
  });

  describe('#paramToString', function() {
    it('should return empty string for null and undefined', function() {
      expect(apiClient.paramToString(null)).to.be('');
      expect(apiClient.paramToString(undefined)).to.be('');
    });

    it('should return string', function() {
      expect(apiClient.paramToString('')).to.be('');
      expect(apiClient.paramToString('abc')).to.be('abc');
      expect(apiClient.paramToString(123)).to.be('123');
    });
  });

  describe('#buildCollectionParam', function() {
    var param;

    beforeEach(function() {
      param = ['aa', 'bb', 123];
    });

    it('works for csv', function() {
      expect(apiClient.buildCollectionParam(param, 'csv')).to.be('aa,bb,123');
    });

    it('works for ssv', function() {
      expect(apiClient.buildCollectionParam(param, 'ssv')).to.be('aa bb 123');
    });

    it('works for tsv', function() {
      expect(apiClient.buildCollectionParam(param, 'tsv')).to.be('aa\tbb\t123');
    });

    it('works for pipes', function() {
      expect(apiClient.buildCollectionParam(param, 'pipes')).to.be('aa|bb|123');
    });

    it('works for multi', function() {
      expect(apiClient.buildCollectionParam(param, 'multi')).to.eql(['aa', 'bb', '123']);
    });

    it('fails for invalid collection format', function() {
      expect(function() { apiClient.buildCollectionParam(param, 'INVALID'); }).to.throwError();
    });
  });

  describe('#buildUrl', function() {
    it('should work without path parameters in the path', function() {
      expect(apiClient.buildUrl('/abc', {})).to
        .be('http://petstore.swagger.io:80/v2/abc');
      expect(apiClient.buildUrl('/abc/def?ok', {id: 123})).to
        .be('http://petstore.swagger.io:80/v2/abc/def?ok');
    });

    it('should work with path parameters in the path', function() {
      expect(apiClient.buildUrl('/{id}', {id: 123})).to
        .be('http://petstore.swagger.io:80/v2/123');
      expect(apiClient.buildUrl('/abc/{id}/{name}?ok', {id: 456, name: 'a b'})).to.
        be('http://petstore.swagger.io:80/v2/abc/456/a%20b?ok');
    });
  });

  describe('#isJsonMime', function() {
    it('should return true for JSON MIME', function() {
      expect(apiClient.isJsonMime('application/json')).to.be(true);
      expect(apiClient.isJsonMime('application/json; charset=UTF8')).to.be(true);
      expect(apiClient.isJsonMime('APPLICATION/JSON')).to.be(true);
    });

    it('should return false for non-JSON MIME', function() {
      expect(apiClient.isJsonMime('')).to.be(false);
      expect(apiClient.isJsonMime('text/plain')).to.be(false);
      expect(apiClient.isJsonMime('application/xml')).to.be(false);
      expect(apiClient.isJsonMime('application/jsonp')).to.be(false);
    });
  });

  describe('#applyAuthToRequest', function() {
    var req, newClient;

    beforeEach(function() {
      req = {
        auth: function() {},
        set: function() {},
        query: function() {}
      };
      sinon.stub(req, 'auth');
      sinon.stub(req, 'set');
      sinon.stub(req, 'query');
      newClient = new SwaggerPetstore.ApiClient();
    });

    describe('basic', function() {
      var authName = 'testBasicAuth';
      var authNames = [authName];
      var auth;

      beforeEach(function() {
        newClient.authentications[authName] = {type: 'basic'};
        auth = newClient.authentications[authName];
      });

      it('sets auth header with username and password set', function() {
        auth.username = 'user';
        auth.password = 'pass';
        newClient.applyAuthToRequest(req, authNames);
        sinon.assert.calledOnce(req.auth);
        // 'dXNlcjpwYXNz' is base64-encoded string of 'user:pass'
        sinon.assert.calledWithMatch(req.auth, 'user', 'pass');
        sinon.assert.notCalled(req.set);
        sinon.assert.notCalled(req.query);
      });

      it('sets header with only username set', function() {
        auth.username = 'user';
        newClient.applyAuthToRequest(req, authNames);
        sinon.assert.calledOnce(req.auth);
        // 'dXNlcjo=' is base64-encoded string of 'user:'
        sinon.assert.calledWithMatch(req.auth, 'user', '');
        sinon.assert.notCalled(req.set);
        sinon.assert.notCalled(req.query);
      });

      it('sets header with only password set', function() {
        auth.password = 'pass';
        newClient.applyAuthToRequest(req, authNames);
        sinon.assert.calledOnce(req.auth);
        // 'OnBhc3M=' is base64-encoded string of ':pass'
        sinon.assert.calledWithMatch(req.auth, '', 'pass');
        sinon.assert.notCalled(req.set);
        sinon.assert.notCalled(req.query);
      });

      it('does not set header when username and password are not set', function() {
        newClient.applyAuthToRequest(req, authNames);
        sinon.assert.notCalled(req.auth);
        sinon.assert.notCalled(req.set);
        sinon.assert.notCalled(req.query);
      });
    });

    describe('apiKey', function() {
      var authName = 'testApiKey';
      var authNames = [authName];
      var auth;

      beforeEach(function() {
        newClient.authentications[authName] = {type: 'apiKey', name: 'api_key'};
        auth = newClient.authentications[authName];
      });

      it('sets api key in header', function() {
        auth.in = 'header';
        auth.apiKey = 'my-api-key';
        newClient.applyAuthToRequest(req, authNames);
        sinon.assert.calledOnce(req.set);
        sinon.assert.calledWithMatch(req.set, {'api_key': 'my-api-key'});
        sinon.assert.notCalled(req.auth);
        sinon.assert.notCalled(req.query);
      });

      it('sets api key in query', function() {
        auth.in = 'query';
        auth.apiKey = 'my-api-key';
        newClient.applyAuthToRequest(req, authNames);
        sinon.assert.calledOnce(req.query);
        sinon.assert.calledWithMatch(req.query, {'api_key': 'my-api-key'});
        sinon.assert.notCalled(req.auth);
        sinon.assert.notCalled(req.set);
      });

      it('sets api key in header with prefix', function() {
        auth.in = 'header';
        auth.apiKey = 'my-api-key';
        auth.apiKeyPrefix = 'Key';
        newClient.applyAuthToRequest(req, authNames);
        sinon.assert.calledOnce(req.set);
        sinon.assert.calledWithMatch(req.set, {'api_key': 'Key my-api-key'});
        sinon.assert.notCalled(req.auth);
        sinon.assert.notCalled(req.query);
      });

      it('works when api key is not set', function() {
        auth.in = 'query';
        auth.apiKey = null;
        newClient.applyAuthToRequest(req, authNames);
        sinon.assert.notCalled(req.query);
        sinon.assert.notCalled(req.auth);
        sinon.assert.notCalled(req.set);
      });
    });

    describe('oauth2', function() {
      var authName = 'testOAuth2';
      var authNames = [authName];
      var auth;

      beforeEach(function() {
        newClient.authentications[authName] = {type: 'oauth2'};
        auth = newClient.authentications[authName];
      });

      it('sets access token in header', function() {
        auth.accessToken = 'my-access-token';
        newClient.applyAuthToRequest(req, authNames);
        sinon.assert.calledOnce(req.set);
        sinon.assert.calledWithMatch(req.set, {'Authorization': 'Bearer my-access-token'});
        sinon.assert.notCalled(req.auth);
        sinon.assert.notCalled(req.query);
      });

      it('works when access token is not set', function() {
        auth.accessToken = null;
        newClient.applyAuthToRequest(req, authNames);
        sinon.assert.notCalled(req.query);
        sinon.assert.notCalled(req.auth);
        sinon.assert.notCalled(req.set);
      });
    });

    describe('apiKey and oauth2', function() {
      var apiKeyAuthName = 'testApiKey';
      var oauth2Name = 'testOAuth2';
      var authNames = [apiKeyAuthName, oauth2Name];
      var apiKeyAuth, oauth2;

      beforeEach(function() {
        newClient.authentications[apiKeyAuthName] = {type: 'apiKey', name: 'api_key', 'in': 'query'};
        newClient.authentications[oauth2Name] = {type: 'oauth2'};
        apiKeyAuth = newClient.authentications[apiKeyAuthName];
        oauth2 = newClient.authentications[oauth2Name];
      });

      it('works when setting both api key and access token', function() {
        apiKeyAuth.apiKey = 'my-api-key';
        oauth2.accessToken = 'my-access-token';
        newClient.applyAuthToRequest(req, authNames);
        sinon.assert.calledOnce(req.query);
        sinon.assert.calledWithMatch(req.query, {'api_key': 'my-api-key'});
        sinon.assert.calledOnce(req.set);
        sinon.assert.calledWithMatch(req.set, {'Authorization': 'Bearer my-access-token'});
        sinon.assert.notCalled(req.auth);
      });

      it('works when setting only api key', function() {
        apiKeyAuth.apiKey = 'my-api-key';
        oauth2.accessToken = null;
        newClient.applyAuthToRequest(req, authNames);
        sinon.assert.calledOnce(req.query);
        sinon.assert.calledWithMatch(req.query, {'api_key': 'my-api-key'});
        sinon.assert.notCalled(req.set);
        sinon.assert.notCalled(req.auth);
      });

      it('works when neither api key nor access token is set', function() {
        apiKeyAuth.apiKey = null;
        oauth2.accessToken = null;
        newClient.applyAuthToRequest(req, authNames);
        sinon.assert.notCalled(req.query);
        sinon.assert.notCalled(req.auth);
        sinon.assert.notCalled(req.set);
      });
    });

    describe('unknown type', function() {
      var authName = 'unknown';
      var authNames = [authName];

      beforeEach(function() {
        newClient.authentications[authName] = {type: 'UNKNOWN'};
      });

      it('throws error for unknown auth type', function() {
        expect(function() {
          newClient.applyAuthToRequest(req, authNames);
        }).to.throwError();
        sinon.assert.notCalled(req.set);
        sinon.assert.notCalled(req.auth);
        sinon.assert.notCalled(req.query);
      });
    });
  });

  /*
  describe('#defaultHeaders', function() {
    it('should initialize default headers to be an empty object', function() {
      expect(apiClient.defaultHeaders).to.eql({});
    });

    it('should put default headers in request', function() {
      var newClient = new SwaggerPetstore.ApiClient;
      newClient.defaultHeaders['Content-Type'] = 'text/plain'
      newClient.defaultHeaders['api_key'] = 'special-key'

      var expected = {'Content-Type': 'text/plain', 'api_key': 'special-key'};
      expect(newClient.defaultHeaders).to.eql(expected);
      var req = makeDumbRequest(newClient);
      req.unset('User-Agent');
      expect(req.header).to.eql(expected);
    });

    it('should override default headers with provided header params', function() {
      var newClient = new SwaggerPetstore.ApiClient;
      newClient.defaultHeaders['Content-Type'] = 'text/plain'
      newClient.defaultHeaders['api_key'] = 'special-key'

      var headerParams = {'Content-Type': 'application/json', 'Authorization': 'Bearer test-token'}
      var expected = {
        'Content-Type': 'application/json',
        'api_key': 'special-key',
        'Authorization': 'Bearer test-token'
      };
      var req = makeDumbRequest(newClient, {headerParams: headerParams});
      req.unset('User-Agent');
      expect(req.header).to.eql(expected);
    });
  });
*/

});

function makeDumbRequest(apiClient, opts) {
  opts = opts || {};
  var path = opts.path || '/store/inventory';
  var httpMethod = opts.httpMethod || 'GET';
  var pathParams = opts.pathParams || {};
  var queryParams = opts.queryParams || {};
  var headerParams = opts.headerParams || {};
  var formParams = opts.formParams || {};
  var bodyParam = opts.bodyParam;
  var authNames = [];
  var contentTypes = opts.contentTypes || [];
  var accepts = opts.accepts || [];
  var callback = opts.callback;
  return apiClient.callApi(path, httpMethod, pathParams, queryParams,
    headerParams, formParams, bodyParam, authNames, contentTypes, accepts);
}
