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
  
  
  var Pet = function Pet(photoUrls, name) { 
    
    /**
     * datatype: String
     * required 
     **/
    this['name'] = name;
    /**
     * datatype: [String]
     * required 
     **/
    this['photoUrls'] = photoUrls;
  };

  Pet.constructFromObject = function(data) {
    if (!data) {
      return null;
    }
    var _this = new Pet();
    
    if (data['id']) {
      _this['id'] = ApiClient.convertToType(data['id'], 'Integer');
    }
    
    if (data['category']) {
      _this['category'] = Category.constructFromObject(data['category']);
    }
    
    if (data['name']) {
      _this['name'] = ApiClient.convertToType(data['name'], 'String');
    }
    
    if (data['photoUrls']) {
      _this['photoUrls'] = ApiClient.convertToType(data['photoUrls'], ['String']);
    }
    
    if (data['tags']) {
      _this['tags'] = ApiClient.convertToType(data['tags'], [Tag]);
    }
    
    if (data['status']) {
      _this['status'] = ApiClient.convertToType(data['status'], 'String');
    }
    
    return _this;
  }

  
  
  /**
   * @return {Integer}
   **/
  Pet.prototype.getId = function() {
    return this['id'];
  }

  /**
   * @param {Integer} id
   **/
  Pet.prototype.setId = function(id) {
    this['id'] = id;
  }
  
  /**
   * @return {Category}
   **/
  Pet.prototype.getCategory = function() {
    return this['category'];
  }

  /**
   * @param {Category} category
   **/
  Pet.prototype.setCategory = function(category) {
    this['category'] = category;
  }
  
  /**
   * @return {String}
   **/
  Pet.prototype.getName = function() {
    return this['name'];
  }

  /**
   * @param {String} name
   **/
  Pet.prototype.setName = function(name) {
    this['name'] = name;
  }
  
  /**
   * @return {[String]}
   **/
  Pet.prototype.getPhotoUrls = function() {
    return this['photoUrls'];
  }

  /**
   * @param {[String]} photoUrls
   **/
  Pet.prototype.setPhotoUrls = function(photoUrls) {
    this['photoUrls'] = photoUrls;
  }
  
  /**
   * @return {[Tag]}
   **/
  Pet.prototype.getTags = function() {
    return this['tags'];
  }

  /**
   * @param {[Tag]} tags
   **/
  Pet.prototype.setTags = function(tags) {
    this['tags'] = tags;
  }
  
  /**
   * get pet status in the store
   * @return {StatusEnum}
   **/
  Pet.prototype.getStatus = function() {
    return this['status'];
  }

  /**
   * set pet status in the store
   * @param {StatusEnum} status
   **/
  Pet.prototype.setStatus = function(status) {
    this['status'] = status;
  }
  
  

  Pet.prototype.toJson = function() {
    return JSON.stringify(this);
  }

  var StatusEnum = {

	  /**
	   * @const
	   */
	  AVAILABLE: "available",
	  
	  /**
	   * @const
	   */
	  PENDING: "pending",
	  
	  /**
	   * @const
	   */
	  SOLD: "sold"
  };

  Pet.StatusEnum = StatusEnum;


  if (module) {
    module.Pet = Pet;
  }

  return Pet;
  
  
}));
