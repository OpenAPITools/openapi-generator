'use strict';

var url = require('url');

var Store = require('./StoreService');

module.exports.deleteOrder = function deleteOrder (req, res, next) {
  Store.deleteOrder(req.swagger.params, res, next);
};

module.exports.getInventory = function getInventory (req, res, next) {
  Store.getInventory(req.swagger.params, res, next);
};

module.exports.getOrderById = function getOrderById (req, res, next) {
  Store.getOrderById(req.swagger.params, res, next);
};

module.exports.placeOrder = function placeOrder (req, res, next) {
  Store.placeOrder(req.swagger.params, res, next);
};
