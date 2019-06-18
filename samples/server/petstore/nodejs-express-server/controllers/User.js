const Controller = require('./Controller');

class User {
  constructor(Service) {
    this.service = Service;
  }

  async loginUser(request, response) {
    await Controller.handleRequest(request, response, this.service.loginUser);
  }

  async logoutUser(request, response) {
    await Controller.handleRequest(request, response, this.service.logoutUser);
  }

  async deleteUser(request, response) {
    await Controller.handleRequest(request, response, this.service.deleteUser);
  }

  async getUserByName(request, response) {
    await Controller.handleRequest(request, response, this.service.getUserByName);
  }

  async updateUser(request, response) {
    await Controller.handleRequest(request, response, this.service.updateUser);
  }
}

module.exports = User;
