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

  var getProperty = function(object, getter, property) {
    // Use getter method if present; otherwise, get the property directly.
    if (typeof object[getter] === 'function')
      return object[getter]();
    else
      return object[property];
  }

  var setProperty = function(object, setter, property, value) {
    // Use setter method if present; otherwise, set the property directly.
    if (typeof object[setter] === 'function')
      object[setter](value);
    else
      object[property] = value;
  }

  var createRandomPet = function() {
    var id = new Date().getTime();
    var pet = new SwaggerPetstore.Pet();
    setProperty(pet, "setId", "id", id);
    setProperty(pet, "setName", "name", "pet" + id);

    var category = new SwaggerPetstore.Category();
    setProperty(category, "setId", "id", id);
    setProperty(category, "setName", "name", "category" + id);
    setProperty(pet, "setCategory", "category", category);

    setProperty(pet, "setStatus", "status", "available");
    var photos = ["http://foo.bar.com/1", "http://foo.bar.com/2"];
    setProperty(pet, "setPhotoUrls", "photoUrls", photos);

    return pet;
  };

  describe('PetApi', function() {
    it('should create and get pet', function(done) {
      var pet = createRandomPet();
      api.addPet(pet, function(error) {
        if (error) throw error;

        api.getPetById(pet.id, function(error, fetched, response) {
          if (error) throw error;
          expect(response.status).to.be(200);
          expect(response.ok).to.be(true);
          expect(response.get('Content-Type')).to.be('application/json');

          expect(fetched).to.be.a(SwaggerPetstore.Pet);
          expect(fetched.id).to.be(pet.id);
          expect(getProperty(fetched, "getPhotoUrls", "photoUrls"))
            .to.eql(getProperty(pet, "getPhotoUrls", "photoUrls"));
          expect(getProperty(fetched, "getCategory", "category"))
            .to.be.a(SwaggerPetstore.Category);
          expect(getProperty(getProperty(fetched, "getCategory", "category"), "getName", "name"))
            .to.be(getProperty(getProperty(pet, "getCategory", "category"), "getName", "name"));

          api.deletePet(pet.id);
          done();
        });
      });
    });

    /* commented out the following as the fake endpoint has been removed from the spec
     * we'll add it back after updating the Petstore server
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
          expect(categoryObj.id)
            .to.be(getProperty(getProperty(pet, "getCategory", "category"), "getId", "id"));
          expect(categoryObj.name)
            .to.be(getProperty(getProperty(pet, "getCategory", "category"), "getName", "name"));

          api.deletePet(pet.id);
          done();
        });
      });
    });
    */
  });

}));
