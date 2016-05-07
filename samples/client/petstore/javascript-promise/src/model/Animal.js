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
    root.SwaggerPetstore.Animal = factory(root.SwaggerPetstore.ApiClient);
  }
}(this, function(ApiClient) {
  'use strict';




  /**
   * The Animal model module.
   * @module model/Animal
   * @version 1.0.0
   */

  /**
   * Constructs a new <code>Animal</code>.
   * @alias module:model/Animal
   * @class
   * @param className
   */
  var exports = function(className) {
    var _this = this;

    _this['className'] = className;
  };

  /**
   * Constructs a <code>Animal</code> from a plain JavaScript object, optionally creating a new instance.
   * Copies all relevant properties from <code>data</code> to <code>obj</code> if supplied or a new instance if not.
   * @param {Object} data The plain JavaScript object bearing properties of interest.
   * @param {module:model/Animal} obj Optional instance to populate.
   * @return {module:model/Animal} The populated <code>Animal</code> instance.
   */
  exports.constructFromObject = function(data, obj) {
    if (data) {
      obj = obj || new exports();

      if (data.hasOwnProperty('className')) {
        obj['className'] = ApiClient.convertToType(data['className'], 'String');
      }
    }
    return obj;
  }

  /**
   * @member {String} className
   */
  exports.prototype['className'] = undefined;




  return exports;
}));


