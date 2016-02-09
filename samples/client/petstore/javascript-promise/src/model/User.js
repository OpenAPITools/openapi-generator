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
    
    /**
     * datatype: Integer
     **/
    this['id'] = null;
    
    /**
     * datatype: String
     **/
    this['username'] = null;
    
    /**
     * datatype: String
     **/
    this['firstName'] = null;
    
    /**
     * datatype: String
     **/
    this['lastName'] = null;
    
    /**
     * datatype: String
     **/
    this['email'] = null;
    
    /**
     * datatype: String
     **/
    this['password'] = null;
    
    /**
     * datatype: String
     **/
    this['phone'] = null;
    
    /**
     * User Status
     * datatype: Integer
     **/
    this['userStatus'] = null;
    
  };

  User.prototype.constructFromObject = function(data) {
    if (!data) {
      return this;
    }
    
    this['id'] = ApiClient.convertToType(data['id'], 'Integer');
    
    this['username'] = ApiClient.convertToType(data['username'], 'String');
    
    this['firstName'] = ApiClient.convertToType(data['firstName'], 'String');
    
    this['lastName'] = ApiClient.convertToType(data['lastName'], 'String');
    
    this['email'] = ApiClient.convertToType(data['email'], 'String');
    
    this['password'] = ApiClient.convertToType(data['password'], 'String');
    
    this['phone'] = ApiClient.convertToType(data['phone'], 'String');
    
    this['userStatus'] = ApiClient.convertToType(data['userStatus'], 'Integer');
    
    return this;
  }

  
  /**
   * @return {Integer}
   **/
  User.prototype.getId = function() {
    return this['id'];
  }

  /**
   * @param {Integer} id
   **/
  User.prototype.setId = function(id) {
    this['id'] = id;
  }
  
  /**
   * @return {String}
   **/
  User.prototype.getUsername = function() {
    return this['username'];
  }

  /**
   * @param {String} username
   **/
  User.prototype.setUsername = function(username) {
    this['username'] = username;
  }
  
  /**
   * @return {String}
   **/
  User.prototype.getFirstName = function() {
    return this['firstName'];
  }

  /**
   * @param {String} firstName
   **/
  User.prototype.setFirstName = function(firstName) {
    this['firstName'] = firstName;
  }
  
  /**
   * @return {String}
   **/
  User.prototype.getLastName = function() {
    return this['lastName'];
  }

  /**
   * @param {String} lastName
   **/
  User.prototype.setLastName = function(lastName) {
    this['lastName'] = lastName;
  }
  
  /**
   * @return {String}
   **/
  User.prototype.getEmail = function() {
    return this['email'];
  }

  /**
   * @param {String} email
   **/
  User.prototype.setEmail = function(email) {
    this['email'] = email;
  }
  
  /**
   * @return {String}
   **/
  User.prototype.getPassword = function() {
    return this['password'];
  }

  /**
   * @param {String} password
   **/
  User.prototype.setPassword = function(password) {
    this['password'] = password;
  }
  
  /**
   * @return {String}
   **/
  User.prototype.getPhone = function() {
    return this['phone'];
  }

  /**
   * @param {String} phone
   **/
  User.prototype.setPhone = function(phone) {
    this['phone'] = phone;
  }
  
  /**
   * get User Status
   * @return {Integer}
   **/
  User.prototype.getUserStatus = function() {
    return this['userStatus'];
  }

  /**
   * set User Status
   * @param {Integer} userStatus
   **/
  User.prototype.setUserStatus = function(userStatus) {
    this['userStatus'] = userStatus;
  }
  

  User.prototype.toJson = function() {
    return JSON.stringify(this);
  }

  if (module) {
    module.User = User;
  }

  return User;
  
  
}));
