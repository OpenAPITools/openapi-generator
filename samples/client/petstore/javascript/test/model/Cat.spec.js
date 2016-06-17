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
    instance = new SwaggerPetstore.Cat();
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

  describe('Cat', function() {
    it('should create an instance of Cat', function() {
      // uncomment below and update the code to test Cat
      //var instane = new SwaggerPetstore.Cat();
      //expect(instance).to.be.a(SwaggerPetstore.Cat);
    });

    it('should have the property declawed (base name: "declawed")', function() {
      // uncomment below and update the code to test the property declawed
      //var instane = new SwaggerPetstore.Cat();
      //expect(instance).to.be();
    });

  });

}));
