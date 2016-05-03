(function(root, factory) {
  if (typeof define === 'function' && define.amd) {
    // AMD. Register as an anonymous module.
    define(['ApiClient', 'model/Animal'], factory);
  } else if (typeof module === 'object' && module.exports) {
    // CommonJS-like environments that support module.exports, like Node.
    module.exports = factory(require('../ApiClient'), require('./Animal'));
  } else {
    // Browser globals (root is window)
    if (!root.SwaggerPetstore) {
      root.SwaggerPetstore = {};
    }
    root.SwaggerPetstore.Dog = factory(root.SwaggerPetstore.ApiClient, root.SwaggerPetstore.Animal);
  }
}(this, function(ApiClient, Animal) {
  'use strict';




  /**
   * The Dog model module.
   * @module model/Dog
   * @version 1.0.0
   */

  /**
   * Constructs a new <code>Dog</code>.
   * @alias module:model/Dog
   * @class
   * @extends module:model/Animal
   * @param className
   */
  var exports = function(className) {
    var _this = this;
    Animal.call(_this, className);

  };

  /**
   * Constructs a <code>Dog</code> from a plain JavaScript object, optionally creating a new instance.
   * Copies all relevant properties from <code>data</code> to <code>obj</code> if supplied or a new instance if not.
   * @param {Object} data The plain JavaScript object bearing properties of interest.
   * @param {module:model/Dog} obj Optional instance to populate.
   * @return {module:model/Dog} The populated <code>Dog</code> instance.
   */
  exports.constructFromObject = function(data, obj) {
    if (data) {
      obj = obj || new exports();
      Animal.constructFromObject(data, obj);
      if (data.hasOwnProperty('breed')) {
        obj['breed'] = ApiClient.convertToType(data['breed'], 'String');
      }
    }
    return obj;
  }

  exports.prototype = Object.create(Animal.prototype);
  exports.prototype.constructor = exports;

  /**
   * @member {String} breed
   */
  exports.prototype['breed'] = undefined;




  return exports;
}));


