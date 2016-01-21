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
});
