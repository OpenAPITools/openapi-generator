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
    root.SwaggerPetstore.SpecialModelName = factory(root.SwaggerPetstore.ApiClient);
  }
}(this, function(ApiClient) {
  'use strict';
  
  
  var SpecialModelName = function SpecialModelName() { 
    
  };

  SpecialModelName.constructFromObject = function(data) {
    if (!data) {
      return null;
    }
    var _this = new SpecialModelName();
    
    if (data['$special[property.name]']) {
      _this['$special[property.name]'] = ApiClient.convertToType(data['$special[property.name]'], 'Integer');
    }
    
    return _this;
  }

  
  
  /**
   * @return {Integer}
   **/
  SpecialModelName.prototype.getSpecialPropertyName = function() {
    return this['$special[property.name]'];
  }

  /**
   * @param {Integer} specialPropertyName
   **/
  SpecialModelName.prototype.setSpecialPropertyName = function(specialPropertyName) {
    this['$special[property.name]'] = specialPropertyName;
  }
  
  

  

  return SpecialModelName;
  
  
}));
