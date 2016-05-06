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
    root.SwaggerPetstore.Model200Response = factory(root.SwaggerPetstore.ApiClient);
  }
}(this, function(ApiClient) {
  'use strict';




  /**
   * The Model200Response model module.
   * @module model/Model200Response
   * @version 1.0.0
   */

  /**
   * Constructs a new <code>Model200Response</code>.
   * Model for testing model name starting with number
   * @alias module:model/Model200Response
   * @class
   */
  var exports = function() {
    var _this = this;


  };

  /**
   * Constructs a <code>Model200Response</code> from a plain JavaScript object, optionally creating a new instance.
   * Copies all relevant properties from <code>data</code> to <code>obj</code> if supplied or a new instance if not.
   * @param {Object} data The plain JavaScript object bearing properties of interest.
   * @param {module:model/Model200Response} obj Optional instance to populate.
   * @return {module:model/Model200Response} The populated <code>Model200Response</code> instance.
   */
  exports.constructFromObject = function(data, obj) {
    if (data) {
      obj = obj || new exports();

      if (data.hasOwnProperty('name')) {
        obj['name'] = ApiClient.convertToType(data['name'], 'Integer');
      }
    }
    return obj;
  }

  /**
   * @member {Integer} name
   */
  exports.prototype['name'] = undefined;




  return exports;
}));


