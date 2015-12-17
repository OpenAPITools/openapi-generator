if (typeof module === 'object' && module.exports) {
  var expect = require('expect.js');
  var requireWithMocks = require('./helper.js').requireWithMocks;
  var PetApi = requireWithMocks('../src/api/PetApi');
}

describe('PetApi', function() {
  describe('#getPetById', function () {
    it('should work', function (done) {
      var api = new PetApi();
      api.getPetById(1, function(pet, textStatus, jqXHR) {
        expect(textStatus).to.be('success');
        expect(pet.id).to.be(1);
        done();
      });
    });
  });
});
