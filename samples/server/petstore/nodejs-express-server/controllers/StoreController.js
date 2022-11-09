/**
 * The StoreController file is a very simple one, which does not need to be changed manually,
 * unless there's a case where business logic routes the request to an entity which is not
 * the service.
 * The heavy lifting of the Controller item is done in Request.js - that is where request
 * parameters are extracted and sent to the service, and where response is handled.
 */

const Controller = require('./Controller');
const service = require('../services/StoreService');
const deleteOrder = async (request, response) => {
  await Controller.handleRequest(request, response, service.deleteOrder);
};

const getInventory = async (request, response) => {
  await Controller.handleRequest(request, response, service.getInventory);
};

const getOrderById = async (request, response) => {
  await Controller.handleRequest(request, response, service.getOrderById);
};

const placeOrder = async (request, response) => {
  await Controller.handleRequest(request, response, service.placeOrder);
};


module.exports = {
  deleteOrder,
  getInventory,
  getOrderById,
  placeOrder,
};
