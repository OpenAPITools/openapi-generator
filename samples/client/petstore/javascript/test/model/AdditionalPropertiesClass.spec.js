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
    instance = new SwaggerPetstore.AdditionalPropertiesClass();
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

  describe('AdditionalPropertiesClass', function() {
    it('should create an instance of AdditionalPropertiesClass', function() {
      // uncomment below and update the code to test AdditionalPropertiesClass
      //var instane = new SwaggerPetstore.AdditionalPropertiesClass();
      //expect(instance).to.be.a(SwaggerPetstore.AdditionalPropertiesClass);
    });

    it('should have the property mapProperty (base name: "map_property")', function() {
      // uncomment below and update the code to test the property mapProperty
      //var instane = new SwaggerPetstore.AdditionalPropertiesClass();
      //expect(instance).to.be();
    });

    it('should have the property mapOfMapProperty (base name: "map_of_map_property")', function() {
      // uncomment below and update the code to test the property mapOfMapProperty
      //var instane = new SwaggerPetstore.AdditionalPropertiesClass();
      //expect(instance).to.be();
    });

  });

}));
