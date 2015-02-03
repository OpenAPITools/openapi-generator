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

exports.getOrderById = {
  'spec': {
    "description" : "Operations about pets",
    "path" : "/store/order/{orderId}",
    "notes" : "For valid response try integer IDs with value <= 5. Anything above 5 or nonintegers will generate API errors",
    "summary" : "Find purchase order by ID",
    "method": "GET",
    "parameters" : [].concat([params.path("orderId", "ID of pet that needs to be fetched")]).concat([]).concat([]),
    "type" : "Order",
    "responseMessages" : [errors.invalid('id'), errors.notFound('Order')],
    "nickname" : "getOrderById"
  },
  'action': function (req,res) {
    if (!req.params.orderId) {
      throw errors.invalid('orderId');
    }
    writeResponse(res, {message: "how about implementing getOrderById as a GET method?"});    
  }
};
exports.deleteOrder = {
  'spec': {
    "description" : "Operations about pets",
    "path" : "/store/order/{orderId}",
    "notes" : "For valid response try integer IDs with value < 1000.  Anything above 1000 or nonintegers will generate API errors",
    "summary" : "Delete purchase order by ID",
    "method": "DELETE",
    "parameters" : [].concat([params.path("orderId", "ID of the order that needs to be deleted")]).concat([]).concat([]),
    "type" : "",
    "responseMessages" : [errors.invalid('id'), errors.notFound('')],
    "nickname" : "deleteOrder"
  },
  'action': function (req,res) {
    if (!req.params.orderId) {
      throw errors.invalid('orderId');
    }
    writeResponse(res, {message: "how about implementing deleteOrder as a DELETE method?"});    
  }
};
exports.placeOrder = {
  'spec': {
    "description" : "Operations about pets",
    "path" : "/store/order",
    "notes" : "",
    "summary" : "Place an order for a pet",
    "method": "POST",
    "parameters" : [].concat([]).concat([]).concat([params.body("body", "Order", "order placed for purchasing the pet", true)
    ]),
    "type" : "",
    "responseMessages" : [errors.invalid('id'), errors.notFound('')],
    "nickname" : "placeOrder"
  },
  'action': function (req,res) {
    if (!req.params.body) {
      throw errors.invalid('body');
    }
    writeResponse(res, {message: "how about implementing placeOrder as a POST method?"});    
  }
};

