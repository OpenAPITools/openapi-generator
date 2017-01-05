'use strict';

exports.createUser = function(args, res, next) {
  /**
   * parameters expected in the args:
  * body (User)
  **/
  // no response value expected for this operation
  res.end();
}

exports.createUsersWithArrayInput = function(args, res, next) {
  /**
   * parameters expected in the args:
  * body (List)
  **/
  // no response value expected for this operation
  res.end();
}

exports.createUsersWithListInput = function(args, res, next) {
  /**
   * parameters expected in the args:
  * body (List)
  **/
  // no response value expected for this operation
  res.end();
}

exports.deleteUser = function(args, res, next) {
  /**
   * parameters expected in the args:
  * username (String)
  **/
  // no response value expected for this operation
  res.end();
}

exports.getUserByName = function(args, res, next) {
  /**
   * parameters expected in the args:
  * username (String)
  **/
    var examples = {};
  examples['application/json'] = {
  "firstName" : "aeiou",
  "lastName" : "aeiou",
  "password" : "aeiou",
  "userStatus" : 123,
  "phone" : "aeiou",
  "id" : 123456789,
  "email" : "aeiou",
  "username" : "aeiou"
};
  if(Object.keys(examples).length > 0) {
    res.setHeader('Content-Type', 'application/json');
    res.end(JSON.stringify(examples[Object.keys(examples)[0]] || {}, null, 2));
  }
  else {
    res.end();
  }
  
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
  // no response value expected for this operation
  res.end();
}

exports.updateUser = function(args, res, next) {
  /**
   * parameters expected in the args:
  * username (String)
  * body (User)
  **/
  // no response value expected for this operation
  res.end();
}

