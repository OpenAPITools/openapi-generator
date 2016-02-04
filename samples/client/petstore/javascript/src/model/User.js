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

  
  

  
  var User = function User() { 
    var self = this;
    
    /**
     * datatype: Integer
     **/
    self['id'] = null;
    
    /**
     * datatype: String
     **/
    self['username'] = null;
    
    /**
     * datatype: String
     **/
    self['firstName'] = null;
    
    /**
     * datatype: String
     **/
    self['lastName'] = null;
    
    /**
     * datatype: String
     **/
    self['email'] = null;
    
    /**
     * datatype: String
     **/
    self['password'] = null;
    
    /**
     * datatype: String
     **/
    self['phone'] = null;
    
    /**
     * User Status
     * datatype: Integer
     **/
    self['userStatus'] = null;
    

    self.constructFromObject = function(data) {
      if (!data) {
        return this;
      }
      
      self['id'] = ApiClient.convertToType(data['id'], 'Integer');
      
      self['username'] = ApiClient.convertToType(data['username'], 'String');
      
      self['firstName'] = ApiClient.convertToType(data['firstName'], 'String');
      
      self['lastName'] = ApiClient.convertToType(data['lastName'], 'String');
      
      self['email'] = ApiClient.convertToType(data['email'], 'String');
      
      self['password'] = ApiClient.convertToType(data['password'], 'String');
      
      self['phone'] = ApiClient.convertToType(data['phone'], 'String');
      
      self['userStatus'] = ApiClient.convertToType(data['userStatus'], 'Integer');
      
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
    self.getUsername = function() {
      return self['username'];
    }

    /**
     * @param {String} username
     **/
    self.setUsername = function(username) {
      self['username'] = username;
    }
    
    /**
     * @return {String}
     **/
    self.getFirstName = function() {
      return self['firstName'];
    }

    /**
     * @param {String} firstName
     **/
    self.setFirstName = function(firstName) {
      self['firstName'] = firstName;
    }
    
    /**
     * @return {String}
     **/
    self.getLastName = function() {
      return self['lastName'];
    }

    /**
     * @param {String} lastName
     **/
    self.setLastName = function(lastName) {
      self['lastName'] = lastName;
    }
    
    /**
     * @return {String}
     **/
    self.getEmail = function() {
      return self['email'];
    }

    /**
     * @param {String} email
     **/
    self.setEmail = function(email) {
      self['email'] = email;
    }
    
    /**
     * @return {String}
     **/
    self.getPassword = function() {
      return self['password'];
    }

    /**
     * @param {String} password
     **/
    self.setPassword = function(password) {
      self['password'] = password;
    }
    
    /**
     * @return {String}
     **/
    self.getPhone = function() {
      return self['phone'];
    }

    /**
     * @param {String} phone
     **/
    self.setPhone = function(phone) {
      self['phone'] = phone;
    }
    
    /**
     * get User Status
     * @return {Integer}
     **/
    self.getUserStatus = function() {
      return self['userStatus'];
    }

    /**
     * set User Status
     * @param {Integer} userStatus
     **/
    self.setUserStatus = function(userStatus) {
      self['userStatus'] = userStatus;
    }
    

    self.toJson = function() {
      return JSON.stringify(self);
    }
  };

  if (module) {
    module.User = User;
  }

  return User;
  
  
}));
