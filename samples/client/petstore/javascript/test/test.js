var assert = require('assert');
var mockrequire = require('mockrequire');
var najax = require('najax');

var PetApi = mockrequire('../src/api/PetApi', {
  'jquery': {
    'ajax': najax
  }
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
