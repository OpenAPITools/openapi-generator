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
    api = new SwaggerPetstore.StoreApi();
  });

  describe('StoreApi', function() {
    /* commented out the following as the fake endpoint has been removed from the spec
     * we'll add it back after updating the petstore server
     *
    it('getInventoryInObject', function(done) {
      api.getInventoryInObject(function(error, obj) {
        if (error) throw error;

        expect(obj).to.be.a(Object);
        var hasKey = false;
        for (var key in obj) {
          if (obj.hasOwnProperty(key)) {
            hasKey = true;
            expect(obj[key]).to.be.a('number');
          }
        }
        expect(hasKey).to.be(true);

        done();
      });
    });
    */
  });

}));
