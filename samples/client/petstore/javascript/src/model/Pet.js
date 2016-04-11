(function(root, factory) {
  if (typeof define === 'function' && define.amd) {
    // AMD. Register as an anonymous module.
    define(['../ApiClient', './Category', './Tag'], factory);
  } else if (typeof module === 'object' && module.exports) {
    // CommonJS-like environments that support module.exports, like Node.
    module.exports = factory(require('../ApiClient'), require('./Category'), require('./Tag'));
  } else {
    // Browser globals (root is window)
    if (!root.SwaggerPetstore) {
      root.SwaggerPetstore = {};
    }
    root.SwaggerPetstore.Pet = factory(root.SwaggerPetstore.ApiClient, root.SwaggerPetstore.Category, root.SwaggerPetstore.Tag);
  }
}(this, function(ApiClient, Category, Tag) {
  'use strict';

  /**
   * The Pet model module.
   * @module model/Pet
   * @version 1.0.0
   */

  /**
   * Constructs a new <code>Pet</code>.
   * @alias module:model/Pet
   * @class
   * @param name
   * @param photoUrls
   */
  var exports = function(name, photoUrls) {



    this['name'] = name;
    this['photoUrls'] = photoUrls;


  };

  /**
   * Constructs a <code>Pet</code> from a plain JavaScript object, optionally creating a new instance.
   * Copies all relevant properties from <code>data</code> to <code>obj</code> if supplied or a new instance if not.
   * @param {Object} data The plain JavaScript object bearing properties of interest.
   * @param {module:model/Pet} obj Optional instance to populate.
   * @return {module:model/Pet} The populated <code>Pet</code> instance.
   */
  exports.constructFromObject = function(data, obj) {
    if (data) { 
      obj = obj || new exports();

      if (data.hasOwnProperty('id')) {
        obj['id'] = ApiClient.convertToType(data['id'], 'Integer');
      }
      if (data.hasOwnProperty('category')) {
        obj['category'] = Category.constructFromObject(data['category']);
      }
      if (data.hasOwnProperty('name')) {
        obj['name'] = ApiClient.convertToType(data['name'], 'String');
      }
      if (data.hasOwnProperty('photoUrls')) {
        obj['photoUrls'] = ApiClient.convertToType(data['photoUrls'], ['String']);
      }
      if (data.hasOwnProperty('tags')) {
        obj['tags'] = ApiClient.convertToType(data['tags'], [Tag]);
      }
      if (data.hasOwnProperty('status')) {
        obj['status'] = ApiClient.convertToType(data['status'], 'String');
      }
    }
    return obj;
  }


  /**
   * @member {Integer} id
   */
  exports.prototype['id'] = undefined;

  /**
   * @member {module:model/Category} category
   */
  exports.prototype['category'] = undefined;

  /**
   * @member {String} name
   */
  exports.prototype['name'] = undefined;

  /**
   * @member {Array.<String>} photoUrls
   */
  exports.prototype['photoUrls'] = undefined;

  /**
   * @member {Array.<module:model/Tag>} tags
   */
  exports.prototype['tags'] = undefined;

  /**
   * pet status in the store
   * @member {module:model/Pet.StatusEnum} status
   */
  exports.prototype['status'] = undefined;


  /**
   * Allowed values for the <code>status</code> property.
   * @enum {String}
   * @readonly
   */
  exports.StatusEnum = { 
    /**
     * value: available
     * @const
     */
    AVAILABLE: "available",
    
    /**
     * value: pending
     * @const
     */
    PENDING: "pending",
    
    /**
     * value: sold
     * @const
     */
    SOLD: "sold"
  };

  return exports;
}));
