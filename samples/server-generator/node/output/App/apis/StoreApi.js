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

exports.getOrderById = {
  'spec': {
    "description" : "Operations about pets",
    "path" : "/store.{format}/order/{orderId}",
    "notes" : "For valid response try integer IDs with value <= 5. Anything above 5 or nonintegers will generate API errors",
    "summary" : "Find purchase order by ID",
    "method": "GET",
    "params" : [].concat([swagger.pathParam("orderId", "ID of pet that needs to be fetched")]).concat([]).concat([]),
    "responseClass" : "Order",
    "errorResponses" : [errors.invalid('id'), errors.notFound('Order')],
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
    "path" : "/store.{format}/order/{orderId}",
    "notes" : "For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors",
    "summary" : "Delete purchase order by ID",
    "method": "DELETE",
    "params" : [].concat([swagger.pathParam("orderId", "ID of the order that needs to be deleted")]).concat([]).concat([]),
    "responseClass" : "",
    "errorResponses" : [errors.invalid('id'), errors.notFound('')],
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
    "path" : "/store.{format}/order",
    "notes" : "",
    "summary" : "Place an order for a pet",
    "method": "POST",
    "params" : [].concat([]).concat([]).concat([swagger.postParam("Order", "order placed for purchasing the pet", true)
    ]),
    "responseClass" : "",
    "errorResponses" : [errors.invalid('id'), errors.notFound('')],
    "nickname" : "placeOrder"
  },
  'action': function (req,res) {
    if (!req.params.body) {
      throw errors.invalid('body');
    }
    writeResponse(res, {message: "how about implementing placeOrder as a POST method?"});    
  }
};

