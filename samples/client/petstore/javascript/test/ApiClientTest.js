if (typeof module === 'object' && module.exports) {
  var expect = require('expect.js');
  var SwaggerPetstore = require('../src/index');
}

var apiClient = SwaggerPetstore.ApiClient.default;

describe('ApiClient', function() {
  describe('defaults', function() {
    it('should have correct default values with the default API client', function() {
      expect(apiClient).to.be.ok();
      expect(apiClient.basePath).to.be('http://petstore.swagger.io/v2');
    });

    it('should have correct default values with new API client and can customize it', function() {
      var newClient = new SwaggerPetstore.ApiClient;
      expect(newClient.basePath).to.be('http://petstore.swagger.io/v2');
      expect(newClient.buildUrl('/abc', {})).to.be('http://petstore.swagger.io/v2/abc');

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
        .be('http://petstore.swagger.io/v2/abc');
      expect(apiClient.buildUrl('/abc/def?ok', {id: 123})).to
        .be('http://petstore.swagger.io/v2/abc/def?ok');
    });

    it('should work with path parameters in the path', function() {
      expect(apiClient.buildUrl('/{id}', {id: 123})).to
        .be('http://petstore.swagger.io/v2/123');
      expect(apiClient.buildUrl('/abc/{id}/{name}?ok', {id: 456, name: 'a b'})).to.
        be('http://petstore.swagger.io/v2/abc/456/a%20b?ok');
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
  var contentTypes = opts.contentTypes || [];
  var accepts = opts.accepts || [];
  var callback = opts.callback;
  return apiClient.callApi(path, httpMethod, pathParams, queryParams,
    headerParams, formParams, bodyParam, contentTypes, accepts, callback
  );
}
