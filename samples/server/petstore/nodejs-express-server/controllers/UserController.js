/**
 * The UserController file is a very simple one, which does not need to be changed manually,
 * unless there's a case where business logic reoutes the request to an entity which is not
 * the service.
 * The heavy lifting of the Controller item is done in Request.js - that is where request
 * parameters are extracted and sent to the service, and where response is handled.
 */

const Controller = require('./Controller');
const service = require('../services/UserService');
const createUser = async (request, response) => {
  await Controller.handleRequest(request, response, service.createUser);
};

const createUsersWithArrayInput = async (request, response) => {
  await Controller.handleRequest(request, response, service.createUsersWithArrayInput);
};

const createUsersWithListInput = async (request, response) => {
  await Controller.handleRequest(request, response, service.createUsersWithListInput);
};

const deleteUser = async (request, response) => {
  await Controller.handleRequest(request, response, service.deleteUser);
};

const getUserByName = async (request, response) => {
  await Controller.handleRequest(request, response, service.getUserByName);
};

const loginUser = async (request, response) => {
  await Controller.handleRequest(request, response, service.loginUser);
};

const logoutUser = async (request, response) => {
  await Controller.handleRequest(request, response, service.logoutUser);
};

const updateUser = async (request, response) => {
  await Controller.handleRequest(request, response, service.updateUser);
};


module.exports = {
  createUser,
  createUsersWithArrayInput,
  createUsersWithListInput,
  deleteUser,
  getUserByName,
  loginUser,
  logoutUser,
  updateUser,
};
