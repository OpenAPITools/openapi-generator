var assert = require('assert');
var mockrequire = require('mockrequire');

var jquery = require('jquery');
var domino = require('domino');
var XMLHttpRequest = require('xmlhttprequest').XMLHttpRequest;
var window = domino.createWindow();
var $ = jquery(window);
$.support.cors = true;
$.ajaxSettings.xhr = function() {
  return new XMLHttpRequest();
};

var PetApi = mockrequire('../src/api/PetApi', {
  'jquery': $
});

describe('PetApi', function() {
  describe('#getPetById', function () {
    it('should work', function (done) {
      var api = new PetApi();
      api.getPetById(1, function(pet, textStatus, jqXHR) {
        assert.equal('success', textStatus);
        assert.equal(1, pet.id);
        done();
      });
    });
  });
});
