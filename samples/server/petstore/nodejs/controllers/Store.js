'use strict';

var url = require('url');


module.exports.getInventory = function getInventory (req, res, next) {
  

  console.log('do some magic!');
  res.setHeader('Content-Type', 'application/json');
  res.end();
};

module.exports.placeOrder = function placeOrder (req, res, next) {
  var body = req.swagger.params['body'].value;
  

  console.log('do some magic!');
  res.setHeader('Content-Type', 'application/json');
  res.end();
};

module.exports.getOrderById = function getOrderById (req, res, next) {
  var orderId = req.swagger.params['orderId'].value;
  

  console.log('do some magic!');
  res.setHeader('Content-Type', 'application/json');
  res.end();
};

module.exports.deleteOrder = function deleteOrder (req, res, next) {
  var orderId = req.swagger.params['orderId'].value;
  

  console.log('do some magic!');
  res.setHeader('Content-Type', 'application/json');
  res.end();
};
