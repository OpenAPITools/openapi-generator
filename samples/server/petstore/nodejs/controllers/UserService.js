'use strict';

exports.createUser = function(body) {

  var examples = {};
  

  
}
exports.createUsersWithArrayInput = function(body) {

  var examples = {};
  

  
}
exports.createUsersWithListInput = function(body) {

  var examples = {};
  

  
}
exports.loginUser = function(username, password) {

  var examples = {};
  
  examples['application/json'] = "aeiou";
  

  
  if(Object.keys(examples).length > 0)
    return examples[Object.keys(examples)[0]];
  
}
exports.logoutUser = function() {

  var examples = {};
  

  
}
exports.getUserByName = function(username) {

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
  

  
  if(Object.keys(examples).length > 0)
    return examples[Object.keys(examples)[0]];
  
}
exports.updateUser = function(username, body) {

  var examples = {};
  

  
}
exports.deleteUser = function(username) {

  var examples = {};
  

  
}
