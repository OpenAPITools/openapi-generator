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

exports.getInventory = {
  'spec': {
    "description" : "Operations about pets",
    "path" : "/store/inventory",
    "notes" : "Returns a map of status codes to quantities",
    "summary" : "Returns pet inventories by status",
    "method": "GET",
    "params" : [].concat([]).concat([]).concat([]),
    
    "type": "Map",
    "items": {
      
      "$ref": "map"
    },
    // container
    
    
    "responseMessages" : [errors.invalid('id'), errors.notFound('Map')],
    "nickname" : "getInventory"
  },
  'action': function (req,res) {
    
    writeResponse(res, {message: "how about implementing getInventory as a GET method?"});    
  }
};
exports.placeOrder = {
  'spec': {
    "description" : "Operations about pets",
    "path" : "/store/order",
    "notes" : "",
    "summary" : "Place an order for a pet",
    "method": "POST",
    "params" : [].concat([]).concat([]).concat([
      params.body("body", "", "order placed for purchasing the pet", false)
    ]),
    
    
    "type" : "Order",
    
    "responseMessages" : [errors.invalid('id'), errors.notFound('Order')],
    "nickname" : "placeOrder"
  },
  'action': function (req,res) {
    
    writeResponse(res, {message: "how about implementing placeOrder as a POST method?"});    
  }
};
exports.getOrderById = {
  'spec': {
    "description" : "Operations about pets",
    "path" : "/store/order/{orderId}",
    "notes" : "For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions",
    "summary" : "Find purchase order by ID",
    "method": "GET",
    "params" : [].concat([
      params.path("orderId", "ID of pet that needs to be fetched")
    ]).concat([]).concat([]),
    
    
    "type" : "Order",
    
    "responseMessages" : [errors.invalid('id'), errors.notFound('Order')],
    "nickname" : "getOrderById"
  },
  'action': function (req,res) {
    
    writeResponse(res, {message: "how about implementing getOrderById as a GET method?"});    
  }
};
exports.deleteOrder = {
  'spec': {
    "description" : "Operations about pets",
    "path" : "/store/order/{orderId}",
    "notes" : "For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors",
    "summary" : "Delete purchase order by ID",
    "method": "DELETE",
    "params" : [].concat([
      params.path("orderId", "ID of the order that needs to be deleted")
    ]).concat([]).concat([]),
    
    
    "type" : "",
    
    "responseMessages" : [errors.invalid('id'), errors.notFound('')],
    "nickname" : "deleteOrder"
  },
  'action': function (req,res) {
    
    writeResponse(res, {message: "how about implementing deleteOrder as a DELETE method?"});    
  }
};
