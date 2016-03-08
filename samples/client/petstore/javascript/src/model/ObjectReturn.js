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
  
  
  var ObjectReturn = function ObjectReturn() { 
    
  };

  ObjectReturn.constructFromObject = function(data) {
    if (!data) {
      return null;
    }
    var _this = new ObjectReturn();
    
    if (data['return']) {
      _this['return'] = ApiClient.convertToType(data['return'], 'Integer');
    }
    
    return _this;
  }

  
  
  /**
   * @return {Integer}
   **/
  ObjectReturn.prototype.getReturn = function() {
    return this['return'];
  }

  /**
   * @param {Integer} _return
   **/
  ObjectReturn.prototype.setReturn = function(_return) {
    this['return'] = _return;
  }
  
  

  

  if (module) {
    module.ObjectReturn = ObjectReturn;
  }

  return ObjectReturn;
  
  
}));
