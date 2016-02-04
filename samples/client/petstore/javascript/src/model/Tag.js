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

  
  

  
  var Tag = function Tag() { 
    var self = this;
    
    /**
     * datatype: Integer
     **/
    self['id'] = null;
    
    /**
     * datatype: String
     **/
    self['name'] = null;
    

    self.constructFromObject = function(data) {
      if (!data) {
        return this;
      }
      
      self['id'] = ApiClient.convertToType(data['id'], 'Integer');
      
      self['name'] = ApiClient.convertToType(data['name'], 'String');
      
      return this;
    }

    
    /**
     * @return {Integer}
     **/
    self.getId = function() {
      return self['id'];
    }

    /**
     * @param {Integer} id
     **/
    self.setId = function(id) {
      self['id'] = id;
    }
    
    /**
     * @return {String}
     **/
    self.getName = function() {
      return self['name'];
    }

    /**
     * @param {String} name
     **/
    self.setName = function(name) {
      self['name'] = name;
    }
    

    self.toJson = function() {
      return JSON.stringify(self);
    }
  };

  if (module) {
    module.Tag = Tag;
  }

  return Tag;
  
  
}));
