var swagger = require("swagger-node-express");
var url = require("url");
var errors = swagger.errors;

/* add model includes */

function writeResponse (response, data) {
  response.header('Access-Control-Allow-Origin', "*");
  response.header("Access-Control-Allow-Methods", "GET, POST, DELETE, PUT");
  response.header("Access-Control-Allow-Headers", "Content-Type");
  response.header("Content-Type", "application/json; charset=utf-8");
  response.send(JSON.stringify(data));
}

exports.models = models = require("../models.js");

exports.createUsersWithArrayInput = {
  'spec': {
    "description" : "Operations about pets",
    "path" : "/user.{format}/createWithArray",
    "notes" : "",
    "summary" : "Creates list of users with given input array",
    "method": "POST",
    "params" : [].concat([]).concat([]).concat([swagger.postParam("Array[User]", "List of user object", true)
    ]),
    "responseClass" : "",
    "errorResponses" : [errors.invalid('id'), errors.notFound('')],
    "nickname" : "createUsersWithArrayInput"
  },
  'action': function (req,res) {
    if (!req.params.body) {
      throw errors.invalid('body');
    }
    writeResponse(res, {message: "how about implementing createUsersWithArrayInput as a POST method?"});    
  }
};
exports.createUser = {
  'spec': {
    "description" : "Operations about pets",
    "path" : "/user.{format}",
    "notes" : "This can only be done by the logged in user.",
    "summary" : "Create user",
    "method": "POST",
    "params" : [].concat([]).concat([]).concat([swagger.postParam("User", "Created user object", true)
    ]),
    "responseClass" : "",
    "errorResponses" : [errors.invalid('id'), errors.notFound('')],
    "nickname" : "createUser"
  },
  'action': function (req,res) {
    if (!req.params.body) {
      throw errors.invalid('body');
    }
    writeResponse(res, {message: "how about implementing createUser as a POST method?"});    
  }
};
exports.createUsersWithListInput = {
  'spec': {
    "description" : "Operations about pets",
    "path" : "/user.{format}/createWithList",
    "notes" : "",
    "summary" : "Creates list of users with given list input",
    "method": "POST",
    "params" : [].concat([]).concat([]).concat([swagger.postParam("List[User]", "List of user object", true)
    ]),
    "responseClass" : "",
    "errorResponses" : [errors.invalid('id'), errors.notFound('')],
    "nickname" : "createUsersWithListInput"
  },
  'action': function (req,res) {
    if (!req.params.body) {
      throw errors.invalid('body');
    }
    writeResponse(res, {message: "how about implementing createUsersWithListInput as a POST method?"});    
  }
};
exports.updateUser = {
  'spec': {
    "description" : "Operations about pets",
    "path" : "/user.{format}/{username}",
    "notes" : "This can only be done by the logged in user.",
    "summary" : "Updated user",
    "method": "PUT",
    "params" : [].concat([swagger.pathParam("username", "name that need to be deleted")]).concat([]).concat([swagger.postParam("User", "Updated user object", true)
    ]),
    "responseClass" : "",
    "errorResponses" : [errors.invalid('id'), errors.notFound('')],
    "nickname" : "updateUser"
  },
  'action': function (req,res) {
    if (!req.params.username) {
      throw errors.invalid('username');
    }
    if (!req.params.body) {
      throw errors.invalid('body');
    }
    writeResponse(res, {message: "how about implementing updateUser as a PUT method?"});    
  }
};
exports.deleteUser = {
  'spec': {
    "description" : "Operations about pets",
    "path" : "/user.{format}/{username}",
    "notes" : "This can only be done by the logged in user.",
    "summary" : "Delete user",
    "method": "DELETE",
    "params" : [].concat([swagger.pathParam("username", "The name that needs to be deleted")]).concat([]).concat([]),
    "responseClass" : "",
    "errorResponses" : [errors.invalid('id'), errors.notFound('')],
    "nickname" : "deleteUser"
  },
  'action': function (req,res) {
    if (!req.params.username) {
      throw errors.invalid('username');
    }
    writeResponse(res, {message: "how about implementing deleteUser as a DELETE method?"});    
  }
};
exports.getUserByName = {
  'spec': {
    "description" : "Operations about pets",
    "path" : "/user.{format}/{username}",
    "notes" : "",
    "summary" : "Get user by user name",
    "method": "GET",
    "params" : [].concat([swagger.pathParam("username", "The name that needs to be fetched. Use user1 for testing.")]).concat([]).concat([]),
    "responseClass" : "User",
    "errorResponses" : [errors.invalid('id'), errors.notFound('User')],
    "nickname" : "getUserByName"
  },
  'action': function (req,res) {
    if (!req.params.username) {
      throw errors.invalid('username');
    }
    writeResponse(res, {message: "how about implementing getUserByName as a GET method?"});    
  }
};
exports.loginUser = {
  'spec': {
    "description" : "Operations about pets",
    "path" : "/user.{format}/login",
    "notes" : "",
    "summary" : "Logs user into the system",
    "method": "GET",
    "params" : [swagger.queryParam("username", "The user name for login", "string", true, false, ""),swagger.queryParam("password", "The password for login in clear text", "string", true, false, "")].concat([]).concat([]).concat([]),
    "responseClass" : "String",
    "errorResponses" : [errors.invalid('id'), errors.notFound('String')],
    "nickname" : "loginUser"
  },
  'action': function (req,res) {
    if (!req.params.username) {
      throw errors.invalid('username');
    }
    if (!req.params.password) {
      throw errors.invalid('password');
    }
    writeResponse(res, {message: "how about implementing loginUser as a GET method?"});    
  }
};
exports.logoutUser = {
  'spec': {
    "description" : "Operations about pets",
    "path" : "/user.{format}/logout",
    "notes" : "",
    "summary" : "Logs out current logged in user session",
    "method": "GET",
    "params" : [].concat([]).concat([]).concat([]),
    "responseClass" : "",
    "errorResponses" : [errors.invalid('id'), errors.notFound('')],
    "nickname" : "logoutUser"
  },
  'action': function (req,res) {
    writeResponse(res, {message: "how about implementing logoutUser as a GET method?"});    
  }
};

