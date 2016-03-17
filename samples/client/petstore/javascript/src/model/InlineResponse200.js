(function(root, factory) {
  if (typeof define === 'function' && define.amd) {
    // AMD. Register as an anonymous module.
    define(['../ApiClient', './Tag'], factory);
  } else if (typeof module === 'object' && module.exports) {
    // CommonJS-like environments that support module.exports, like Node.
    module.exports = factory(require('../ApiClient'), require('./Tag'));
  } else {
    // Browser globals (root is window)
    if (!root.SwaggerPetstore) {
      root.SwaggerPetstore = {};
    }
    root.SwaggerPetstore.InlineResponse200 = factory(root.SwaggerPetstore.ApiClient, root.SwaggerPetstore.Tag);
  }
}(this, function(ApiClient, Tag) {
  'use strict';
  
  
  var InlineResponse200 = function InlineResponse200(id) { 
    
    /**
     * datatype: Integer
     * required 
     **/
    this['id'] = id;
  };

  InlineResponse200.constructFromObject = function(data) {
    if (!data) {
      return null;
    }
    var _this = new InlineResponse200();
    
    if (data['photoUrls']) {
      _this['photoUrls'] = ApiClient.convertToType(data['photoUrls'], ['String']);
    }
    
    if (data['name']) {
      _this['name'] = ApiClient.convertToType(data['name'], 'String');
    }
    
    if (data['id']) {
      _this['id'] = ApiClient.convertToType(data['id'], 'Integer');
    }
    
    if (data['category']) {
      _this['category'] = ApiClient.convertToType(data['category'], Object);
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
   * @return {[String]}
   **/
  InlineResponse200.prototype.getPhotoUrls = function() {
    return this['photoUrls'];
  }

  /**
   * @param {[String]} photoUrls
   **/
  InlineResponse200.prototype.setPhotoUrls = function(photoUrls) {
    this['photoUrls'] = photoUrls;
  }
  
  /**
   * @return {String}
   **/
  InlineResponse200.prototype.getName = function() {
    return this['name'];
  }

  /**
   * @param {String} name
   **/
  InlineResponse200.prototype.setName = function(name) {
    this['name'] = name;
  }
  
  /**
   * @return {Integer}
   **/
  InlineResponse200.prototype.getId = function() {
    return this['id'];
  }

  /**
   * @param {Integer} id
   **/
  InlineResponse200.prototype.setId = function(id) {
    this['id'] = id;
  }
  
  /**
   * @return {Object}
   **/
  InlineResponse200.prototype.getCategory = function() {
    return this['category'];
  }

  /**
   * @param {Object} category
   **/
  InlineResponse200.prototype.setCategory = function(category) {
    this['category'] = category;
  }
  
  /**
   * @return {[Tag]}
   **/
  InlineResponse200.prototype.getTags = function() {
    return this['tags'];
  }

  /**
   * @param {[Tag]} tags
   **/
  InlineResponse200.prototype.setTags = function(tags) {
    this['tags'] = tags;
  }
  
  /**
   * get pet status in the store
   * @return {StatusEnum}
   **/
  InlineResponse200.prototype.getStatus = function() {
    return this['status'];
  }

  /**
   * set pet status in the store
   * @param {StatusEnum} status
   **/
  InlineResponse200.prototype.setStatus = function(status) {
    this['status'] = status;
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

  InlineResponse200.StatusEnum = StatusEnum;


  return InlineResponse200;
  
  
}));
