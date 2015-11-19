'use strict';

exports.createUser = function(args, res, next) {
  /**
   * parameters expected in the args:
   * body (User)
   **/

var examples = {};
  

  
  res.end();
}
exports.createUsersWithArrayInput = function(args, res, next) {
  /**
   * parameters expected in the args:
   * body (List)
   **/

var examples = {};
  

  
  res.end();
}
exports.createUsersWithListInput = function(args, res, next) {
  /**
   * parameters expected in the args:
   * body (List)
   **/

var examples = {};
  

  
  res.end();
}
exports.loginUser = function(args, res, next) {
  /**
   * parameters expected in the args:
   * username (String)
   * password (String)
   **/

var examples = {};
  
  examples['application/json'] = "aeiou";
  

  
  if(Object.keys(examples).length > 0) {
    res.setHeader('Content-Type', 'application/json');
    res.end(JSON.stringify(examples[Object.keys(examples)[0]] || {}, null, 2));
  }
  else {
    res.end();
  }
  
  
}
exports.logoutUser = function(args, res, next) {
  /**
   * parameters expected in the args:
   **/

var examples = {};
  

  
  res.end();
}
exports.getUserByName = function(args, res, next) {
  /**
   * parameters expected in the args:
   * username (String)
   **/

var examples = {};
  
  examples['application/json'] = {
  "id" : 1,
  "username" : "johnp",
  "firstName" : "John",
  "lastName" : "Public",
  "email" : "johnp@swagger.io",
  "password" : "-secret-",
  "phone" : "0123456789",
  "userStatus" : 0
};
  

  
  if(Object.keys(examples).length > 0) {
    res.setHeader('Content-Type', 'application/json');
    res.end(JSON.stringify(examples[Object.keys(examples)[0]] || {}, null, 2));
  }
  else {
    res.end();
  }
  
  
}
exports.updateUser = function(args, res, next) {
  /**
   * parameters expected in the args:
   * username (String)
   * body (User)
   **/

var examples = {};
  

  
  res.end();
}
exports.deleteUser = function(args, res, next) {
  /**
   * parameters expected in the args:
   * username (String)
   **/

var examples = {};
  

  
  res.end();
}
