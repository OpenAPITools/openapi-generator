(function(root, factory) {
  if (typeof define === 'function' && define.amd) {
    // AMD. Register as an anonymous module.
    define([undefined, '../ApiClient', './Category', './Tag'], factory);
  } else if (typeof module === 'object' && module.exports) {
    // CommonJS-like environments that support module.exports, like Node.
    module.exports = factory(undefined, require('../ApiClient'), require('./Category'), require('./Tag'));
  } else {
    // Browser globals (root is window)
    if (!root.SwaggerPetstore) {
      root.SwaggerPetstore = {};
    }
    factory(root.SwaggerPetstore, root.SwaggerPetstore.ApiClient, root.SwaggerPetstore.Category, root.SwaggerPetstore.Tag);
  }
}(this, function(module, ApiClient, Category, Tag) {
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
    self['id'] = null;
    
    /**
     * datatype: Category
     **/
    self['category'] = new Category();
    
    /**
     * datatype: String
     * required
     **/
    self['name'] = name;
    
    /**
     * datatype: [String]
     * required
     **/
    self['photoUrls'] = photoUrls;
    
    /**
     * datatype: [Tag]
     **/
    self['tags'] = [];
    
    /**
     * pet status in the store
     * datatype: StatusEnum
     **/
    self['status'] = null;
    

    self.constructFromObject = function(data) {
      if (!data) {
        return this;
      }
      
      self['id'] = ApiClient.convertToType(data['id'], 'Integer');
      
      self['category'].constructFromObject(data['category']);
      
      self['name'] = ApiClient.convertToType(data['name'], 'String');
      
      self['photoUrls'] = ApiClient.convertToType(data['photoUrls'], ['String']);
      
      self['tags'] = ApiClient.convertToType(data['tags'], [Tag]);
      
      self['status'] = ApiClient.convertToType(data['status'], 'String');
      
      return this;
    }

    
    /**
     * @return {Integer}
     **/
    self.getId = function() {
      return self['id'];
    }

    /**
     * @param {Integer} id
     **/
    self.setId = function(id) {
      self['id'] = id;
    }
    
    /**
     * @return {Category}
     **/
    self.getCategory = function() {
      return self['category'];
    }

    /**
     * @param {Category} category
     **/
    self.setCategory = function(category) {
      self['category'] = category;
    }
    
    /**
     * @return {String}
     **/
    self.getName = function() {
      return self['name'];
    }

    /**
     * @param {String} name
     **/
    self.setName = function(name) {
      self['name'] = name;
    }
    
    /**
     * @return {[String]}
     **/
    self.getPhotoUrls = function() {
      return self['photoUrls'];
    }

    /**
     * @param {[String]} photoUrls
     **/
    self.setPhotoUrls = function(photoUrls) {
      self['photoUrls'] = photoUrls;
    }
    
    /**
     * @return {[Tag]}
     **/
    self.getTags = function() {
      return self['tags'];
    }

    /**
     * @param {[Tag]} tags
     **/
    self.setTags = function(tags) {
      self['tags'] = tags;
    }
    
    /**
     * get pet status in the store
     * @return {StatusEnum}
     **/
    self.getStatus = function() {
      return self['status'];
    }

    /**
     * set pet status in the store
     * @param {StatusEnum} status
     **/
    self.setStatus = function(status) {
      self['status'] = status;
    }
    

    self.toJson = function() {
      return JSON.stringify(self);
    }
  };

  if (module) {
    module.Pet = Pet;
  }

  return Pet;
  
  
}));
