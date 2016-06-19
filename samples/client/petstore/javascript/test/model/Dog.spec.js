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

  var instance;

  beforeEach(function() {
    instance = new SwaggerPetstore.Dog();
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

  describe('Dog', function() {
    it('should create an instance of Dog', function() {
      // uncomment below and update the code to test Dog
      //var instane = new SwaggerPetstore.Dog();
      //expect(instance).to.be.a(SwaggerPetstore.Dog);
    });

    it('should have the property breed (base name: "breed")', function() {
      // uncomment below and update the code to test the property breed
      //var instane = new SwaggerPetstore.Dog();
      //expect(instance).to.be();
    });

  });

}));
