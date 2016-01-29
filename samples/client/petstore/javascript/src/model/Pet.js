(function(root, factory) {
  if (typeof define === 'function' && define.amd) {
    // AMD. Register as an anonymous module.
    define([undefined, './Category', './Tag'], factory);
  } else if (typeof module === 'object' && module.exports) {
    // CommonJS-like environments that support module.exports, like Node.
    module.exports = factory(undefined, require('./Category'), require('./Tag'));
  } else {
    // Browser globals (root is window)
    if (!root.SwaggerPetstore) {
      root.SwaggerPetstore = {};
    }
    factory(root.SwaggerPetstore, root.SwaggerPetstore.Category, root.SwaggerPetstore.Tag);
  }
}(this, function(module, Category, Tag) {
  'use strict';

  
  
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
	self.AVAILABLE = "available",
	
	/**
	 * @const 
	 */ 
	self.PENDING = "pending",
	
	/**
	 * @const 
	 */ 
	self.SOLD = "sold";

}


  
  var Pet = function Pet(photoUrls, name) { 
    var self = this;
    
    /**
     * datatype: Integer
     **/
    self.id = null;
    
    /**
     * datatype: Category
     **/
    self.category = new Category();
    
    /**
     * datatype: String
     * required
     **/
    self.name = name;
    
    /**
     * datatype: Array
     * required
     **/
    self.photoUrls = photoUrls;
    
    /**
     * datatype: Array
     **/
    self.tags = [];
    
    /**
     * pet status in the store
     * datatype: StatusEnum
     **/
    self.status = null;
    

    self.constructFromObject = function(data) {
      if (!data) {
        return;
      }
      
      self.id = data.id;
      
      self.category.constructFromObject(data.category);
      
      self.name = data.name;
      
      self.photoUrls = data.photoUrls;
      
      self.tags = data.tags;
      
      self.status = data.status;
      
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
     * @return {Category}
     **/
    self.getCategory = function() {
      return self.category;
    }

    /**
     * @param {Category} category
     **/
    self.setCategory = function (category) {
      self.category = category;
    }
    
    /**
     * @return {String}
     **/
    self.getName = function() {
      return self.name;
    }

    /**
     * @param {String} name
     **/
    self.setName = function (name) {
      self.name = name;
    }
    
    /**
     * @return {Array}
     **/
    self.getPhotoUrls = function() {
      return self.photoUrls;
    }

    /**
     * @param {Array} photoUrls
     **/
    self.setPhotoUrls = function (photoUrls) {
      self.photoUrls = photoUrls;
    }
    
    /**
     * @return {Array}
     **/
    self.getTags = function() {
      return self.tags;
    }

    /**
     * @param {Array} tags
     **/
    self.setTags = function (tags) {
      self.tags = tags;
    }
    
    /**
     * get pet status in the store
     * @return {StatusEnum}
     **/
    self.getStatus = function() {
      return self.status;
    }

    /**
     * set pet status in the store
     * @param {StatusEnum} status
     **/
    self.setStatus = function (status) {
      self.status = status;
    }
    

    self.toJson = function () {
      return JSON.stringify(self);
    }
  };

  if (module) {
    module.Pet = Pet;
  }

  return Pet;
  
  
}));
