var swagger = require("swagger-node-express");
var url = require("url");
var errors = swagger.errors;
var params = swagger.params;

/* add model includes */

function writeResponse (response, data) {
  response.header('Access-Control-Allow-Origin', "*");
  response.header("Access-Control-Allow-Methods", "GET, POST, DELETE, PUT");
  response.header("Access-Control-Allow-Headers", "Content-Type");
  response.header("Content-Type", "application/json; charset=utf-8");
  response.send(JSON.stringify(data));
}

exports.models = models = require("../models.js");

exports.createUser = {
  'spec': {
    "description" : "Operations about pets",
    "path" : "/user",
    "notes" : "This can only be done by the logged in user.",
    "summary" : "Create user",
    "method": "POST",
    "params" : [].concat([]).concat([]).concat([
      params.body("body", "", "Created user object", false)
    ]),
    
    
    "type" : "",
    
    "responseMessages" : [errors.invalid('id'), errors.notFound('')],
    "nickname" : "createUser"
  },
  'action': function (req,res) {
    
    writeResponse(res, {message: "how about implementing createUser as a POST method?"});    
  }
};
exports.createUsersWithArrayInput = {
  'spec': {
    "description" : "Operations about pets",
    "path" : "/user/createWithArray",
    "notes" : "",
    "summary" : "Creates list of users with given input array",
    "method": "POST",
    "params" : [].concat([]).concat([]).concat([
      params.body("body", "", "List of user object", false)
    ]),
    
    
    "type" : "",
    
    "responseMessages" : [errors.invalid('id'), errors.notFound('')],
    "nickname" : "createUsersWithArrayInput"
  },
  'action': function (req,res) {
    
    writeResponse(res, {message: "how about implementing createUsersWithArrayInput as a POST method?"});    
  }
};
exports.createUsersWithListInput = {
  'spec': {
    "description" : "Operations about pets",
    "path" : "/user/createWithList",
    "notes" : "",
    "summary" : "Creates list of users with given input array",
    "method": "POST",
    "params" : [].concat([]).concat([]).concat([
      params.body("body", "", "List of user object", false)
    ]),
    
    
    "type" : "",
    
    "responseMessages" : [errors.invalid('id'), errors.notFound('')],
    "nickname" : "createUsersWithListInput"
  },
  'action': function (req,res) {
    
    writeResponse(res, {message: "how about implementing createUsersWithListInput as a POST method?"});    
  }
};
exports.loginUser = {
  'spec': {
    "description" : "Operations about pets",
    "path" : "/user/login",
    "notes" : "",
    "summary" : "Logs user into the system",
    "method": "GET",
    "params" : [
      params.query("username", "The user name for login", "", false, false, ""),
    
      params.query("password", "The password for login in clear text", "", false, false, "")
    ].concat([]).concat([]).concat([]),
    
    
    "type" : "String",
    
    "responseMessages" : [errors.invalid('id'), errors.notFound('String')],
    "nickname" : "loginUser"
  },
  'action': function (req,res) {
    
    writeResponse(res, {message: "how about implementing loginUser as a GET method?"});    
  }
};
exports.logoutUser = {
  'spec': {
    "description" : "Operations about pets",
    "path" : "/user/logout",
    "notes" : "",
    "summary" : "Logs out current logged in user session",
    "method": "GET",
    "params" : [].concat([]).concat([]).concat([]),
    
    
    "type" : "",
    
    "responseMessages" : [errors.invalid('id'), errors.notFound('')],
    "nickname" : "logoutUser"
  },
  'action': function (req,res) {
    
    writeResponse(res, {message: "how about implementing logoutUser as a GET method?"});    
  }
};
exports.getUserByName = {
  'spec': {
    "description" : "Operations about pets",
    "path" : "/user/{username}",
    "notes" : "",
    "summary" : "Get user by user name",
    "method": "GET",
    "params" : [].concat([
      params.path("username", "The name that needs to be fetched. Use user1 for testing. ")
    ]).concat([]).concat([]),
    
    
    "type" : "User",
    
    "responseMessages" : [errors.invalid('id'), errors.notFound('User')],
    "nickname" : "getUserByName"
  },
  'action': function (req,res) {
    
    writeResponse(res, {message: "how about implementing getUserByName as a GET method?"});    
  }
};
exports.updateUser = {
  'spec': {
    "description" : "Operations about pets",
    "path" : "/user/{username}",
    "notes" : "This can only be done by the logged in user.",
    "summary" : "Updated user",
    "method": "PUT",
    "params" : [].concat([
      params.path("username", "name that need to be deleted")
    ]).concat([]).concat([
      params.body("body", "", "Updated user object", false)
    ]),
    
    
    "type" : "",
    
    "responseMessages" : [errors.invalid('id'), errors.notFound('')],
    "nickname" : "updateUser"
  },
  'action': function (req,res) {
    
    writeResponse(res, {message: "how about implementing updateUser as a PUT method?"});    
  }
};
exports.deleteUser = {
  'spec': {
    "description" : "Operations about pets",
    "path" : "/user/{username}",
    "notes" : "This can only be done by the logged in user.",
    "summary" : "Delete user",
    "method": "DELETE",
    "params" : [].concat([
      params.path("username", "The name that needs to be deleted")
    ]).concat([]).concat([]),
    
    
    "type" : "",
    
    "responseMessages" : [errors.invalid('id'), errors.notFound('')],
    "nickname" : "deleteUser"
  },
  'action': function (req,res) {
    
    writeResponse(res, {message: "how about implementing deleteUser as a DELETE method?"});    
  }
};
