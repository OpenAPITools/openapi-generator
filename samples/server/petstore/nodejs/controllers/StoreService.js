'use strict';

exports.getInventory = function() {

  var examples = {};
  
  examples['application/json'] = {
  "key" : 123
};
  

  
  if(Object.keys(examples).length > 0)
    return examples[Object.keys(examples)[0]];
  
}
exports.placeOrder = function(body) {

  var examples = {};
  
  examples['application/json'] = {
  "id" : 123456789,
  "petId" : 123456789,
  "complete" : true,
  "status" : "aeiou",
  "quantity" : 123,
  "shipDate" : "2015-03-19T21:51:51.599+0000"
};
  

  
  if(Object.keys(examples).length > 0)
    return examples[Object.keys(examples)[0]];
  
}
exports.getOrderById = function(orderId) {

  var examples = {};
  
  examples['application/json'] = {
  "id" : 123456789,
  "petId" : 123456789,
  "complete" : true,
  "status" : "aeiou",
  "quantity" : 123,
  "shipDate" : "2015-03-19T21:51:51.603+0000"
};
  

  
  if(Object.keys(examples).length > 0)
    return examples[Object.keys(examples)[0]];
  
}
exports.deleteOrder = function(orderId) {

  var examples = {};
  

  
}
