const Controller = require('./Controller');

class UserController {
  constructor(Service) {
    this.service = Service;
  }

  async createUser(request, response) {
    await Controller.handleRequest(request, response, this.service.createUser);
  }

  async createUsersWithArrayInput(request, response) {
    await Controller.handleRequest(request, response, this.service.createUsersWithArrayInput);
  }

  async createUsersWithListInput(request, response) {
    await Controller.handleRequest(request, response, this.service.createUsersWithListInput);
  }

  async deleteUser(request, response) {
    await Controller.handleRequest(request, response, this.service.deleteUser);
  }

  async getUserByName(request, response) {
    await Controller.handleRequest(request, response, this.service.getUserByName);
  }

  async loginUser(request, response) {
    await Controller.handleRequest(request, response, this.service.loginUser);
  }

  async logoutUser(request, response) {
    await Controller.handleRequest(request, response, this.service.logoutUser);
  }

  async updateUser(request, response) {
    await Controller.handleRequest(request, response, this.service.updateUser);
  }

}

module.exports = UserController;
