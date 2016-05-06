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
   * Model for testing model name same as property name
   * @alias module:model/Name
   * @class
   * @param name
   */
  var exports = function(name) {
    var _this = this;

    _this['name'] = name;


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
      if (data.hasOwnProperty('snake_case')) {
        obj['snake_case'] = ApiClient.convertToType(data['snake_case'], 'Integer');
      }
      if (data.hasOwnProperty('property')) {
        obj['property'] = ApiClient.convertToType(data['property'], 'String');
      }
    }
    return obj;
  }

  /**
   * @member {Integer} name
   */
  exports.prototype['name'] = undefined;
  /**
   * @member {Integer} snake_case
   */
  exports.prototype['snake_case'] = undefined;
  /**
   * @member {String} property
   */
  exports.prototype['property'] = undefined;




  return exports;
}));


