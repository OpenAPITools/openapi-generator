(function(root, factory) {
  if (typeof define === 'function' && define.amd) {
    // AMD. Register as an anonymous module.
    define(['../ApiClient', './Animal'], factory);
  } else if (typeof module === 'object' && module.exports) {
    // CommonJS-like environments that support module.exports, like Node.
    module.exports = factory(require('../ApiClient'), require('./Animal'));
  } else {
    // Browser globals (root is window)
    if (!root.SwaggerPetstore) {
      root.SwaggerPetstore = {};
    }
    root.SwaggerPetstore.Cat = factory(root.SwaggerPetstore.ApiClient, root.SwaggerPetstore.Animal);
  }
}(this, function(ApiClient, Animal) {
  'use strict';

  /**
   * The Cat model module.
   * @module model/Cat
   * @version 1.0.0
   */

  /**
   * Constructs a new <code>Cat</code>.
   * @alias module:model/Cat
   * @class
   * @extends module:model/Animal
   * @param className
   */
  var exports = function(className) {
    Animal.call(this, className);

  };

  /**
   * Constructs a <code>Cat</code> from a plain JavaScript object, optionally creating a new instance.
   * Copies all relevant properties from <code>data</code> to <code>obj</code> if supplied or a new instance if not.
   * @param {Object} data The plain JavaScript object bearing properties of interest.
   * @param {module:model/Cat} obj Optional instance to populate.
   * @return {module:model/Cat} The populated <code>Cat</code> instance.
   */
  exports.constructFromObject = function(data, obj) {
    if (data) { 
      obj = obj || new exports();
      Animal.constructFromObject(data, obj);
      if (data.hasOwnProperty('declawed')) {
        obj['declawed'] = ApiClient.convertToType(data['declawed'], 'Boolean');
      }
    }
    return obj;
  }

  exports.prototype = Object.create(Animal.prototype);
  exports.prototype.constructor = exports;


  /**
   * @member {Boolean} declawed
   */
  exports.prototype['declawed'] = undefined;




  return exports;
}));
