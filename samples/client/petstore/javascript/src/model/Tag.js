(function(root, factory) {
  if (typeof define === 'function' && define.amd) {
    // AMD. Register as an anonymous module.
    define([undefined], factory);
  } else if (typeof module === 'object' && module.exports) {
    // CommonJS-like environments that support module.exports, like Node.
    module.exports = factory(undefined);
  } else {
    // Browser globals (root is window)
    if (!root.SwaggerPetstore) {
      root.SwaggerPetstore = {};
    }
    factory(root.SwaggerPetstore);
  }
}(this, function(module) {
  'use strict';

  
  

  
  var Tag = function Tag() { 
    var self = this;
    
    /**
     * datatype: Integer
     **/
    self.id = null;
    
    /**
     * datatype: String
     **/
    self.name = null;
    

    self.constructFromObject = function(data) {
      if (!data) {
        return;
      }
      
      self.id = data.id;
      
      self.name = data.name;
      
    }

    
    /**
     * @return {Integer}
     **/
    self.getId = function() {
      return self.id;
    }

    /**
     * @param {Integer} id
     **/
    self.setId = function (id) {
      self.id = id;
    }
    
    /**
     * @return {String}
     **/
    self.getName = function() {
      return self.name;
    }

    /**
     * @param {String} name
     **/
    self.setName = function (name) {
      self.name = name;
    }
    

    self.toJson = function () {
      return JSON.stringify(self);
    }
  };

  if (module) {
    module.Tag = Tag;
  }

  return Tag;
  
  
}));
