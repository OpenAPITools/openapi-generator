// require files in Node.js environment

if (typeof module === 'object' && module.exports) {
  
}



//export module
if ( typeof define === "function" && define.amd ) {     
	define('StatusEnum', ['jquery'], function($) {
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


//export module
if ( typeof define === "function" && define.amd ) {
  define('Order', ['jquery'],
    function($) {
      return Order;
   });
}


var Order = function Order() { 
  var self = this;
  
  /**
   * datatype: Integer
   **/
  self.id = null;
  
  /**
   * datatype: Integer
   **/
  self.petId = null;
  
  /**
   * datatype: Integer
   **/
  self.quantity = null;
  
  /**
   * datatype: Date
   **/
  self.shipDate = null;
  
  /**
   * Order Status
   * datatype: StatusEnum
   **/
  self.status = null;
  
  /**
   * datatype: Boolean
   **/
  self.complete = null;
  
  
  self.constructFromObject = function(data) {
    if (!data) {
      return;
    }
    
    self.id = data.id;
    
    self.petId = data.petId;
    
    self.quantity = data.quantity;
    
    self.shipDate = data.shipDate;
    
    self.status = data.status;
    
    self.complete = data.complete;
    
  }

  
  /**
   * @return {Integer}
   **/
  self.getId = function() {
    return self.id;
  }

  /**
   * @param {Integer} id
   **/
  self.setId = function (id) {
    self.id = id;
  }
  
  /**
   * @return {Integer}
   **/
  self.getPetId = function() {
    return self.petId;
  }

  /**
   * @param {Integer} petId
   **/
  self.setPetId = function (petId) {
    self.petId = petId;
  }
  
  /**
   * @return {Integer}
   **/
  self.getQuantity = function() {
    return self.quantity;
  }

  /**
   * @param {Integer} quantity
   **/
  self.setQuantity = function (quantity) {
    self.quantity = quantity;
  }
  
  /**
   * @return {Date}
   **/
  self.getShipDate = function() {
    return self.shipDate;
  }

  /**
   * @param {Date} shipDate
   **/
  self.setShipDate = function (shipDate) {
    self.shipDate = shipDate;
  }
  
  /**
   * get Order Status
   * @return {StatusEnum}
   **/
  self.getStatus = function() {
    return self.status;
  }

  /**
   * set Order Status
   * @param {StatusEnum} status
   **/
  self.setStatus = function (status) {
    self.status = status;
  }
  
  /**
   * @return {Boolean}
   **/
  self.getComplete = function() {
    return self.complete;
  }

  /**
   * @param {Boolean} complete
   **/
  self.setComplete = function (complete) {
    self.complete = complete;
  }
  

  self.toJson = function () {
    return JSON.stringify(self);
  }
}

if (typeof module === 'object' && module.exports) {
  module.exports = Order;
}
