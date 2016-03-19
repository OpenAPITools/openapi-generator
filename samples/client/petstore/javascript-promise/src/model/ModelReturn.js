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
    root.SwaggerPetstore.ModelReturn = factory(root.SwaggerPetstore.ApiClient);
  }
}(this, function(ApiClient) {
  'use strict';

  /**
   * The ModelReturn model module.
   * @module model/ModelReturn
   * @version 1.0.0
   */

  /**
   * Constructs a new <code>ModelReturn</code>.
   * @alias module:model/ModelReturn
   * @class
   */
  var exports = function() {


  };

  /**
   * Constructs a <code>ModelReturn</code> from a plain JavaScript object, optionally creating a new instance.
   * Copies all relevant properties from <code>data</code> to <code>obj</code> if supplied or a new instance if not.
   * @param {Object} data The plain JavaScript object bearing properties of interest.
   * @param {module:model/ModelReturn} obj Optional instance to populate.
   * @return {module:model/ModelReturn} The populated <code>ModelReturn</code> instance.
   */
  exports.constructFromObject = function(data, obj) {
    if (data) { 
      obj = obj || new exports();

      if (data.hasOwnProperty('return')) {
        obj['return'] = ApiClient.convertToType(data['return'], 'Integer');
      }
    }
    return obj;
  }


  /**
   * @member {Integer} return
   */
  exports.prototype['return'] = undefined;




  return exports;
}));
