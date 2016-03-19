(function(root, factory) {
  if (typeof define === 'function' && define.amd) {
    // AMD. Register as an anonymous module.
    define(['../ApiClient'], factory);
  } else if (typeof module === 'object' && module.exports) {
    // CommonJS-like environments that support module.exports, like Node.
    module.exports = factory(require('../ApiClient'));
  } else {
    // Browser globals (root is window)
    if (!root.SwaggerPetstore) {
      root.SwaggerPetstore = {};
    }
    root.SwaggerPetstore.Name = factory(root.SwaggerPetstore.ApiClient);
  }
}(this, function(ApiClient) {
  'use strict';

  /**
   * The Name model module.
   * @module model/Name
   * @version 1.0.0
   */

  /**
   * Constructs a new <code>Name</code>.
   * @alias module:model/Name
   * @class
   */
  var exports = function() {


  };

  /**
   * Constructs a <code>Name</code> from a plain JavaScript object, optionally creating a new instance.
   * Copies all relevant properties from <code>data</code> to <code>obj</code> if supplied or a new instance if not.
   * @param {Object} data The plain JavaScript object bearing properties of interest.
   * @param {module:model/Name} obj Optional instance to populate.
   * @return {module:model/Name} The populated <code>Name</code> instance.
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
