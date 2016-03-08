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
    
    if (data['name']) {
      _this['name'] = ApiClient.convertToType(data['name'], 'String');
    }
    
    if (data['id']) {
      _this['id'] = ApiClient.convertToType(data['id'], 'Integer');
    }
    
    if (data['category']) {
      _this['category'] = ApiClient.convertToType(data['category'], Object);
    }
    
    return _this;
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
  
  

  

  if (module) {
    module.InlineResponse200 = InlineResponse200;
  }

  return InlineResponse200;
  
  
}));
