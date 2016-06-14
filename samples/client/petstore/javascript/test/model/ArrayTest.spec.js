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
    instance = new SwaggerPetstore.ArrayTest();
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

  describe('ArrayTest', function() {
    it('should create an instance of ArrayTest', function() {
      // uncomment below and update the code to test ArrayTest
      //var instane = new SwaggerPetstore.ArrayTest();
      //expect(instance).to.be.a(SwaggerPetstore.ArrayTest);
    });

    it('should have the property arrayOfString (base name: "array_of_string")', function() {
      // uncomment below and update the code to test the property arrayOfString
      //var instane = new SwaggerPetstore.ArrayTest();
      //expect(instance).to.be();
    });

    it('should have the property arrayArrayOfInteger (base name: "array_array_of_integer")', function() {
      // uncomment below and update the code to test the property arrayArrayOfInteger
      //var instane = new SwaggerPetstore.ArrayTest();
      //expect(instance).to.be();
    });

    it('should have the property arrayArrayOfModel (base name: "array_array_of_model")', function() {
      // uncomment below and update the code to test the property arrayArrayOfModel
      //var instane = new SwaggerPetstore.ArrayTest();
      //expect(instance).to.be();
    });

  });

}));
