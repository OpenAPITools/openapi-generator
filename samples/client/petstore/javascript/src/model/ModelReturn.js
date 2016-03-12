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
    root.SwaggerPetstore.ModelReturn = factory(root.SwaggerPetstore.ApiClient);
  }
}(this, function(ApiClient) {
  'use strict';
  
  
  var ModelReturn = function ModelReturn() { 
    
  };

  ModelReturn.constructFromObject = function(data) {
    if (!data) {
      return null;
    }
    var _this = new ModelReturn();
    
    if (data['return']) {
      _this['return'] = ApiClient.convertToType(data['return'], 'Integer');
    }
    
    return _this;
  }

  
  
  /**
   * @return {Integer}
   **/
  ModelReturn.prototype.getReturn = function() {
    return this['return'];
  }

  /**
   * @param {Integer} _return
   **/
  ModelReturn.prototype.setReturn = function(_return) {
    this['return'] = _return;
  }
  
  

  

  return ModelReturn;
  
  
}));
