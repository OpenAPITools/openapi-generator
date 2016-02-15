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

  
  

  
  var Category = function Category() { 
    
    /**
     * datatype: Integer
     **/
    this['id'] = null;
    
    /**
     * datatype: String
     **/
    this['name'] = null;
    
  };

  Category.constructFromObject = function(data) {
    if (!data) {
      return null;
    }
    var _this = new Category();
    
    if (data['id']) {
      _this['id'] = ApiClient.convertToType(data['id'], 'Integer');
    }
    
    if (data['name']) {
      _this['name'] = ApiClient.convertToType(data['name'], 'String');
    }
    
    return _this;
  }

  
  
  /**
   * @return {Integer}
   **/
  Category.prototype.getId = function() {
    return this['id'];
  }

  /**
   * @param {Integer} id
   **/
  Category.prototype.setId = function(id) {
    this['id'] = id;
  }
  
  /**
   * @return {String}
   **/
  Category.prototype.getName = function() {
    return this['name'];
  }

  /**
   * @param {String} name
   **/
  Category.prototype.setName = function(name) {
    this['name'] = name;
  }
  
  

  Category.prototype.toJson = function() {
    return JSON.stringify(this);
  }

  if (module) {
    module.Category = Category;
  }

  return Category;
  
  
}));
