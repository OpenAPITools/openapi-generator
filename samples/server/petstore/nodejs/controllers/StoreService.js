'use strict';

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
exports.placeOrder = function(args, res, next) {
  /**
   * parameters expected in the args:
   * body (Order)
   **/

var examples = {};
  
  examples['application/json'] = {
  "id" : 123456789,
  "petId" : 123456789,
  "complete" : true,
  "status" : "aeiou",
  "quantity" : 123,
  "shipDate" : "2015-11-18T02:43:54.540+0000"
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
   * orderId (String)
   **/

var examples = {};
  
  examples['application/json'] = {
  "id" : 123456789,
  "petId" : 123456789,
  "complete" : true,
  "status" : "aeiou",
  "quantity" : 123,
  "shipDate" : "2015-11-18T02:43:54.544+0000"
};
  

  
  if(Object.keys(examples).length > 0) {
    res.setHeader('Content-Type', 'application/json');
    res.end(JSON.stringify(examples[Object.keys(examples)[0]] || {}, null, 2));
  }
  else {
    res.end();
  }
  
  
}
exports.deleteOrder = function(args, res, next) {
  /**
   * parameters expected in the args:
   * orderId (String)
   **/

var examples = {};
  

  
  res.end();
}
