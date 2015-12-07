


//export module
if ( typeof define === "function" && define.amd ) {     
	define('User', ['jquery'], 
		function($) {
        return User;
	 });
}


var User = function User() { 
  	var self = this;
  	
  	/**
	  * datatype: Long
	  **/
	  self.id = null;
	 
  	/**
	  * datatype: String
	  **/
	  self.username = null;
	 
  	/**
	  * datatype: String
	  **/
	  self.firstName = null;
	 
  	/**
	  * datatype: String
	  **/
	  self.lastName = null;
	 
  	/**
	  * datatype: String
	  **/
	  self.email = null;
	 
  	/**
	  * datatype: String
	  **/
	  self.password = null;
	 
  	/**
	  * datatype: String
	  **/
	  self.phone = null;
	 
  	/**
	  * User Status
	  * datatype: Integer
	  **/
	  self.userStatus = null;
	 
  
  	self.constructFromObject = function(data) {
	  	
	  	self.id = data.id;
	  	
	  	self.username = data.username;
	  	
	  	self.firstName = data.firstName;
	  	
	  	self.lastName = data.lastName;
	  	
	  	self.email = data.email;
	  	
	  	self.password = data.password;
	  	
	  	self.phone = data.phone;
	  	
	  	self.userStatus = data.userStatus;
	  	
	}
  
  
  /**
   * @return {Long}
   **/
  self.getId = function() {
    return self.id;
  }
  
  /**
   * @param {Long} id
   **/
  self.setId = function (id) {
    self.id = id;
  }
  
  /**
   * @return {String}
   **/
  self.getUsername = function() {
    return self.username;
  }
  
  /**
   * @param {String} username
   **/
  self.setUsername = function (username) {
    self.username = username;
  }
  
  /**
   * @return {String}
   **/
  self.getFirstName = function() {
    return self.firstName;
  }
  
  /**
   * @param {String} firstName
   **/
  self.setFirstName = function (firstName) {
    self.firstName = firstName;
  }
  
  /**
   * @return {String}
   **/
  self.getLastName = function() {
    return self.lastName;
  }
  
  /**
   * @param {String} lastName
   **/
  self.setLastName = function (lastName) {
    self.lastName = lastName;
  }
  
  /**
   * @return {String}
   **/
  self.getEmail = function() {
    return self.email;
  }
  
  /**
   * @param {String} email
   **/
  self.setEmail = function (email) {
    self.email = email;
  }
  
  /**
   * @return {String}
   **/
  self.getPassword = function() {
    return self.password;
  }
  
  /**
   * @param {String} password
   **/
  self.setPassword = function (password) {
    self.password = password;
  }
  
  /**
   * @return {String}
   **/
  self.getPhone = function() {
    return self.phone;
  }
  
  /**
   * @param {String} phone
   **/
  self.setPhone = function (phone) {
    self.phone = phone;
  }
  
  /**
   * get User Status
   * @return {Integer}
   **/
  self.getUserStatus = function() {
    return self.userStatus;
  }
  
  /**
   * set User Status
   * @param {Integer} userStatus
   **/
  self.setUserStatus = function (userStatus) {
    self.userStatus = userStatus;
  }
  

  self.toJson = function () {
  	return JSON.stringify(self);
  }
}
