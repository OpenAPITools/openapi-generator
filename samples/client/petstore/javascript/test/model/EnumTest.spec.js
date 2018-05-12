(function(root, factory) {
  if (typeof define === 'function' && define.amd) {
    // AMD.
    define(['expect.js', '../../src/index'], factory);
  } else if (typeof module === 'object' && module.exports) {
    // CommonJS-like environments that support module.exports, like Node.
    factory(require('expect.js'), require('../../src/index'));
  } else {
    // Browser globals (root is window)
    factory(root.expect, root.OpenAPIPetstore);
  }
}(this, function(expect, OpenAPIPetstore) {
  'use strict';

  var instance;

  beforeEach(function() {
    instance = new OpenAPIPetstore.EnumTest();
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

  describe('EnumTest', function() {
    it('should create an instance of EnumTest', function() {
      // uncomment below and update the code to test EnumTest
      //var instane = new OpenAPIPetstore.EnumTest();
      //expect(instance).to.be.a(OpenAPIPetstore.EnumTest);
    });

    it('should have the property enumString (base name: "enum_string")', function() {
      // uncomment below and update the code to test the property enumString
      //var instane = new OpenAPIPetstore.EnumTest();
      //expect(instance).to.be();
    });

    it('should have the property enumInteger (base name: "enum_integer")', function() {
      // uncomment below and update the code to test the property enumInteger
      //var instane = new OpenAPIPetstore.EnumTest();
      //expect(instance).to.be();
    });

    it('should have the property enumNumber (base name: "enum_number")', function() {
      // uncomment below and update the code to test the property enumNumber
      //var instane = new OpenAPIPetstore.EnumTest();
      //expect(instance).to.be();
    });

  });

}));
