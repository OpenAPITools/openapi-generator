(function(root, factory) {
  if (typeof define === 'function' && define.amd) {
    // AMD.
    define(['expect.js', '../../src/index'], factory);
  } else if (typeof module === 'object' && module.exports) {
    // CommonJS-like environments that support module.exports, like Node.
    factory(require('expect.js'), require('../../src/index'));
  } else {
    // Browser globals (root is window)
    factory(root.expect, root.SwaggerPetstore);
  }
}(this, function(expect, SwaggerPetstore) {
  'use strict';

  var api;

  beforeEach(function() {
    api = new SwaggerPetstore.PetApi();
  });

  var createRandomPet = function() {
    var id = new Date().getTime();
    var pet = new SwaggerPetstore.Pet();
    pet.setId(id);
    pet.setName("pet" + id);

    var category = new SwaggerPetstore.Category();
    category.setId(id);
    category.setName("category" + id);
    pet.setCategory(category);

    pet.setStatus('available');
    var photos = ["http://foo.bar.com/1", "http://foo.bar.com/2"];
    pet.setPhotoUrls(photos);

    return pet;
  };

  describe('PetApi', function() {
    it('should create and get pet', function(done) {
      var pet = createRandomPet();
      api.addPet({body: pet}, function(error) {
        if (error) throw error;

        api.getPetById(pet.id, function(error, fetched, response) {
          if (error) throw error;
          expect(response.status).to.be(200);
          expect(response.ok).to.be(true);
          expect(response.get('Content-Type')).to.be('application/json');

          expect(fetched).to.be.a(SwaggerPetstore.Pet);
          expect(fetched.id).to.be(pet.id);
          expect(fetched.getPhotoUrls()).to.eql(pet.getPhotoUrls());
          expect(fetched.getCategory()).to.be.a(SwaggerPetstore.Category);
          expect(fetched.getCategory().getName()).to.be(pet.getCategory().getName());

          api.deletePet(pet.id);
          done();
        });
      });
    });

    it('getPetByIdInObject', function(done) {
      var pet = createRandomPet();
      api.addPet({body: pet}, function(error) {
        if (error) throw error;

        api.getPetByIdInObject(pet.id, function(error, fetched) {
          if (error) throw error;

          expect(fetched).to.be.a(SwaggerPetstore.InlineResponse200);
          expect(fetched.id).to.be(pet.id);
          expect(fetched.name).to.be(pet.name);

          var categoryObj = fetched.category;
          expect(categoryObj).to.be.a(Object);
          expect(categoryObj).not.to.be.a(SwaggerPetstore.Category);
          expect(categoryObj.id).to.be(pet.getCategory().getId());
          expect(categoryObj.name).to.be(pet.getCategory().getName());

          api.deletePet(pet.id);
          done();
        });
      });
    });
  });

}));
