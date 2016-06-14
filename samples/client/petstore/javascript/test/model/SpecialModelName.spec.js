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
    instance = new SwaggerPetstore.SpecialModelName();
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

  describe('SpecialModelName', function() {
    it('should create an instance of SpecialModelName', function() {
      // uncomment below and update the code to test SpecialModelName
      //var instane = new SwaggerPetstore.SpecialModelName();
      //expect(instance).to.be.a(SwaggerPetstore.SpecialModelName);
    });

    it('should have the property specialPropertyName (base name: "$special[property.name]")', function() {
      // uncomment below and update the code to test the property specialPropertyName
      //var instane = new SwaggerPetstore.SpecialModelName();
      //expect(instance).to.be();
    });

  });

}));
