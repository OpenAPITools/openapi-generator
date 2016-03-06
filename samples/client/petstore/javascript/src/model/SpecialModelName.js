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
  
  

  

  if (module) {
    module.SpecialModelName = SpecialModelName;
  }

  return SpecialModelName;
  
  
}));
