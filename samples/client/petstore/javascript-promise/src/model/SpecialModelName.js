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
    root.SwaggerPetstore.SpecialModelName = factory(root.SwaggerPetstore.ApiClient);
  }
}(this, function(ApiClient) {
  'use strict';




  /**
   * The SpecialModelName model module.
   * @module model/SpecialModelName
   * @version 1.0.0
   */

  /**
   * Constructs a new <code>SpecialModelName</code>.
   * @alias module:model/SpecialModelName
   * @class
   */
  var exports = function() {
    var _this = this;


  };

  /**
   * Constructs a <code>SpecialModelName</code> from a plain JavaScript object, optionally creating a new instance.
   * Copies all relevant properties from <code>data</code> to <code>obj</code> if supplied or a new instance if not.
   * @param {Object} data The plain JavaScript object bearing properties of interest.
   * @param {module:model/SpecialModelName} obj Optional instance to populate.
   * @return {module:model/SpecialModelName} The populated <code>SpecialModelName</code> instance.
   */
  exports.constructFromObject = function(data, obj) {
    if (data) {
      obj = obj || new exports();

      if (data.hasOwnProperty('$special[property.name]')) {
        obj['$special[property.name]'] = ApiClient.convertToType(data['$special[property.name]'], 'Integer');
      }
    }
    return obj;
  }

  /**
   * @member {Integer} $special[property.name]
   */
  exports.prototype['$special[property.name]'] = undefined;




  return exports;
}));


