(function(root, factory) {
  if (typeof define === 'function' && define.amd) {
    // AMD. Register as an anonymous module.
    define([undefined, '../ApiClient'], factory);
  } else if (typeof module === 'object' && module.exports) {
    // CommonJS-like environments that support module.exports, like Node.
    module.exports = factory(undefined, require('../ApiClient'));
  } else {
    // Browser globals (root is window)
    if (!root.SwaggerPetstore) {
      root.SwaggerPetstore = {};
    }
    factory(root.SwaggerPetstore, root.SwaggerPetstore.ApiClient);
  }
}(this, function(module, ApiClient) {
  'use strict';

  
  
//export module
if ( typeof define === "function" && define.amd ) {
	define('StatusEnum', [], function() {
        return StatusEnum;
	 });
}

var StatusEnum = function StatusEnum() {
	var self = this;


	/**
	 * @const
	 */
	self.PLACED = "placed",
	
	/**
	 * @const
	 */
	self.APPROVED = "approved",
	
	/**
	 * @const
	 */
	self.DELIVERED = "delivered";

}


  
  var Order = function Order() { 
    
    /**
     * datatype: Integer
     **/
    this['id'] = null;
    
    /**
     * datatype: Integer
     **/
    this['petId'] = null;
    
    /**
     * datatype: Integer
     **/
    this['quantity'] = null;
    
    /**
     * datatype: Date
     **/
    this['shipDate'] = null;
    
    /**
     * Order Status
     * datatype: StatusEnum
     **/
    this['status'] = null;
    
    /**
     * datatype: Boolean
     **/
    this['complete'] = null;
    
  };

  Order.prototype.constructFromObject = function(data) {
    if (!data) {
      return this;
    }
    
    this['id'] = ApiClient.convertToType(data['id'], 'Integer');
    
    this['petId'] = ApiClient.convertToType(data['petId'], 'Integer');
    
    this['quantity'] = ApiClient.convertToType(data['quantity'], 'Integer');
    
    this['shipDate'] = ApiClient.convertToType(data['shipDate'], 'Date');
    
    this['status'] = ApiClient.convertToType(data['status'], 'String');
    
    this['complete'] = ApiClient.convertToType(data['complete'], 'Boolean');
    
    return this;
  }

  
  /**
   * @return {Integer}
   **/
  Order.prototype.getId = function() {
    return this['id'];
  }

  /**
   * @param {Integer} id
   **/
  Order.prototype.setId = function(id) {
    this['id'] = id;
  }
  
  /**
   * @return {Integer}
   **/
  Order.prototype.getPetId = function() {
    return this['petId'];
  }

  /**
   * @param {Integer} petId
   **/
  Order.prototype.setPetId = function(petId) {
    this['petId'] = petId;
  }
  
  /**
   * @return {Integer}
   **/
  Order.prototype.getQuantity = function() {
    return this['quantity'];
  }

  /**
   * @param {Integer} quantity
   **/
  Order.prototype.setQuantity = function(quantity) {
    this['quantity'] = quantity;
  }
  
  /**
   * @return {Date}
   **/
  Order.prototype.getShipDate = function() {
    return this['shipDate'];
  }

  /**
   * @param {Date} shipDate
   **/
  Order.prototype.setShipDate = function(shipDate) {
    this['shipDate'] = shipDate;
  }
  
  /**
   * get Order Status
   * @return {StatusEnum}
   **/
  Order.prototype.getStatus = function() {
    return this['status'];
  }

  /**
   * set Order Status
   * @param {StatusEnum} status
   **/
  Order.prototype.setStatus = function(status) {
    this['status'] = status;
  }
  
  /**
   * @return {Boolean}
   **/
  Order.prototype.getComplete = function() {
    return this['complete'];
  }

  /**
   * @param {Boolean} complete
   **/
  Order.prototype.setComplete = function(complete) {
    this['complete'] = complete;
  }
  

  Order.prototype.toJson = function() {
    return JSON.stringify(this);
  }

  if (module) {
    module.Order = Order;
  }

  return Order;
  
  
}));
