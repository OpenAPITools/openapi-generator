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
    root.SwaggerPetstore.Name = factory(root.SwaggerPetstore.ApiClient);
  }
}(this, function(ApiClient) {
  'use strict';
  
  
  var Name = function Name() { 
    
  };

  Name.constructFromObject = function(data) {
    if (!data) {
      return null;
    }
    var _this = new Name();
    
    if (data['name']) {
      _this['name'] = ApiClient.convertToType(data['name'], 'Integer');
    }
    
    return _this;
  }

  
  
  /**
   * @return {Integer}
   **/
  Name.prototype.getName = function() {
    return this['name'];
  }

  /**
   * @param {Integer} name
   **/
  Name.prototype.setName = function(name) {
    this['name'] = name;
  }
  
  

  

  return Name;
  
  
}));
