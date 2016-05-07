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
    root.SwaggerPetstore.Order = factory(root.SwaggerPetstore.ApiClient);
  }
}(this, function(ApiClient) {
  'use strict';




  /**
   * The Order model module.
   * @module model/Order
   * @version 1.0.0
   */

  /**
   * Constructs a new <code>Order</code>.
   * @alias module:model/Order
   * @class
   */
  var exports = function() {
    var _this = this;







  };

  /**
   * Constructs a <code>Order</code> from a plain JavaScript object, optionally creating a new instance.
   * Copies all relevant properties from <code>data</code> to <code>obj</code> if supplied or a new instance if not.
   * @param {Object} data The plain JavaScript object bearing properties of interest.
   * @param {module:model/Order} obj Optional instance to populate.
   * @return {module:model/Order} The populated <code>Order</code> instance.
   */
  exports.constructFromObject = function(data, obj) {
    if (data) {
      obj = obj || new exports();

      if (data.hasOwnProperty('id')) {
        obj['id'] = ApiClient.convertToType(data['id'], 'Integer');
      }
      if (data.hasOwnProperty('petId')) {
        obj['petId'] = ApiClient.convertToType(data['petId'], 'Integer');
      }
      if (data.hasOwnProperty('quantity')) {
        obj['quantity'] = ApiClient.convertToType(data['quantity'], 'Integer');
      }
      if (data.hasOwnProperty('shipDate')) {
        obj['shipDate'] = ApiClient.convertToType(data['shipDate'], 'Date');
      }
      if (data.hasOwnProperty('status')) {
        obj['status'] = ApiClient.convertToType(data['status'], 'String');
      }
      if (data.hasOwnProperty('complete')) {
        obj['complete'] = ApiClient.convertToType(data['complete'], 'Boolean');
      }
    }
    return obj;
  }

  /**
   * @member {Integer} id
   */
  exports.prototype['id'] = undefined;
  /**
   * @member {Integer} petId
   */
  exports.prototype['petId'] = undefined;
  /**
   * @member {Integer} quantity
   */
  exports.prototype['quantity'] = undefined;
  /**
   * @member {Date} shipDate
   */
  exports.prototype['shipDate'] = undefined;
  /**
   * Order Status
   * @member {module:model/Order.StatusEnum} status
   */
  exports.prototype['status'] = undefined;
  /**
   * @member {Boolean} complete
   * @default false
   */
  exports.prototype['complete'] = false;


  /**
   * Allowed values for the <code>status</code> property.
   * @enum {String}
   * @readonly
   */
  exports.StatusEnum = {
    /**
     * value: "placed"
     * @const
     */
    "placed": "placed",
    /**
     * value: "approved"
     * @const
     */
    "approved": "approved",
    /**
     * value: "delivered"
     * @const
     */
    "delivered": "delivered"  };


  return exports;
}));


