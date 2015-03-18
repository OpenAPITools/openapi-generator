'use strict';

var url = require('url');


module.exports.createUser = function createUser (req, res, next) {
  var body = req.swagger.params['body'].value;
  

  console.log('do some magic!');
  res.setHeader('Content-Type', 'application/json');
  res.end();
};

module.exports.createUsersWithArrayInput = function createUsersWithArrayInput (req, res, next) {
  var body = req.swagger.params['body'].value;
  

  console.log('do some magic!');
  res.setHeader('Content-Type', 'application/json');
  res.end();
};

module.exports.createUsersWithListInput = function createUsersWithListInput (req, res, next) {
  var body = req.swagger.params['body'].value;
  

  console.log('do some magic!');
  res.setHeader('Content-Type', 'application/json');
  res.end();
};

module.exports.loginUser = function loginUser (req, res, next) {
  var username = req.swagger.params['username'].value;
  var password = req.swagger.params['password'].value;
  

  console.log('do some magic!');
  res.setHeader('Content-Type', 'application/json');
  res.end();
};

module.exports.logoutUser = function logoutUser (req, res, next) {
  

  console.log('do some magic!');
  res.setHeader('Content-Type', 'application/json');
  res.end();
};

module.exports.getUserByName = function getUserByName (req, res, next) {
  var username = req.swagger.params['username'].value;
  

  console.log('do some magic!');
  res.setHeader('Content-Type', 'application/json');
  res.end();
};

module.exports.updateUser = function updateUser (req, res, next) {
  var username = req.swagger.params['username'].value;
  var body = req.swagger.params['body'].value;
  

  console.log('do some magic!');
  res.setHeader('Content-Type', 'application/json');
  res.end();
};

module.exports.deleteUser = function deleteUser (req, res, next) {
  var username = req.swagger.params['username'].value;
  

  console.log('do some magic!');
  res.setHeader('Content-Type', 'application/json');
  res.end();
};
