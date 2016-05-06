(function(root, factory) {
  if (typeof define === 'function' && define.amd) {
    // AMD. Register as an anonymous module.
    define(['ApiClient'], factory);
  } else if (typeof module === 'object' && module.exports) {
    // CommonJS-like environments that support module.exports, like Node.
    module.exports = factory(require('../ApiClient'));
  } else {
    // Browser globals (root is window)
    if (!root.SwaggerPetstore) {
      root.SwaggerPetstore = {};
    }
    root.SwaggerPetstore.EnumClass = factory(root.SwaggerPetstore.ApiClient);
  }
}(this, function(ApiClient) {
  'use strict';


  /**
   * Enum class EnumClass.
   * @enum {}
   * @readonly
   */
  var exports = {
    /**
     * value: "_abc"
     * @const
     */
    "_abc": "_abc",
    /**
     * value: "-efg"
     * @const
     */
    "-efg": "-efg",
    /**
     * value: "(xyz)"
     * @const
     */
    "(xyz)": "(xyz)"  };

  return exports;
}));


