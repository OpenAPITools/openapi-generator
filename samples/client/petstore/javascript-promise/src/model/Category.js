(function(root, factory) {
  if (typeof define === 'function' && define.amd) {
    // AMD. Register as an anonymous module.
    define(['../ApiClient'], factory);
  } else if (typeof module === 'object' && module.exports) {
    // CommonJS-like environments that support module.exports, like Node.
    module.exports = factory(require('../ApiClient'));
  } else {
    // Browser globals (root is window)
    if (!root.SwaggerPetstore) {
      root.SwaggerPetstore = {};
    }
    root.SwaggerPetstore.Category = factory(root.SwaggerPetstore.ApiClient);
  }
}(this, function(ApiClient) {
  'use strict';
  
  
  var Category = function Category() { 
    
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
  
  

  

  return Category;
  
  
}));
