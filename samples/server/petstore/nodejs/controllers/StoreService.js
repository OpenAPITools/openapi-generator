'use strict';

exports.deleteOrder = function(args, res, next) {
  /**
   * parameters expected in the args:
  * orderId (String)
  **/
  // no response value expected for this operation
  res.end();
}

exports.getInventory = function(args, res, next) {
  /**
   * parameters expected in the args:
  **/
    var examples = {};
  examples['application/json'] = {
  "key" : 123
};
  if(Object.keys(examples).length > 0) {
    res.setHeader('Content-Type', 'application/json');
    res.end(JSON.stringify(examples[Object.keys(examples)[0]] || {}, null, 2));
  }
  else {
    res.end();
  }
  
}

exports.getOrderById = function(args, res, next) {
  /**
   * parameters expected in the args:
  * orderId (Long)
  **/
    var examples = {};
  examples['application/json'] = {
  "petId" : 123456789,
  "quantity" : 123,
  "id" : 123456789,
  "shipDate" : "2000-01-23T04:56:07.000+0000",
  "complete" : true,
  "status" : "aeiou"
};
  if(Object.keys(examples).length > 0) {
    res.setHeader('Content-Type', 'application/json');
    res.end(JSON.stringify(examples[Object.keys(examples)[0]] || {}, null, 2));
  }
  else {
    res.end();
  }
  
}

exports.placeOrder = function(args, res, next) {
  /**
   * parameters expected in the args:
  * body (Order)
  **/
    var examples = {};
  examples['application/json'] = {
  "petId" : 123456789,
  "quantity" : 123,
  "id" : 123456789,
  "shipDate" : "2000-01-23T04:56:07.000+0000",
  "complete" : true,
  "status" : "aeiou"
};
  if(Object.keys(examples).length > 0) {
    res.setHeader('Content-Type', 'application/json');
    res.end(JSON.stringify(examples[Object.keys(examples)[0]] || {}, null, 2));
  }
  else {
    res.end();
  }
  
}

