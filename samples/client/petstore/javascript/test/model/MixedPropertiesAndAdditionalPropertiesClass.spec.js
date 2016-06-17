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
    instance = new SwaggerPetstore.MixedPropertiesAndAdditionalPropertiesClass();
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

  describe('MixedPropertiesAndAdditionalPropertiesClass', function() {
    it('should create an instance of MixedPropertiesAndAdditionalPropertiesClass', function() {
      // uncomment below and update the code to test MixedPropertiesAndAdditionalPropertiesClass
      //var instane = new SwaggerPetstore.MixedPropertiesAndAdditionalPropertiesClass();
      //expect(instance).to.be.a(SwaggerPetstore.MixedPropertiesAndAdditionalPropertiesClass);
    });

    it('should have the property uuid (base name: "uuid")', function() {
      // uncomment below and update the code to test the property uuid
      //var instane = new SwaggerPetstore.MixedPropertiesAndAdditionalPropertiesClass();
      //expect(instance).to.be();
    });

    it('should have the property dateTime (base name: "dateTime")', function() {
      // uncomment below and update the code to test the property dateTime
      //var instane = new SwaggerPetstore.MixedPropertiesAndAdditionalPropertiesClass();
      //expect(instance).to.be();
    });

    it('should have the property map (base name: "map")', function() {
      // uncomment below and update the code to test the property map
      //var instane = new SwaggerPetstore.MixedPropertiesAndAdditionalPropertiesClass();
      //expect(instance).to.be();
    });

  });

}));
