const Controller = require('./Controller');

class StoreController {
  constructor(Service) {
    this.service = Service;
  }

  async deleteOrder(request, response) {
    await Controller.handleRequest(request, response, this.service.deleteOrder);
  }

  async getInventory(request, response) {
    await Controller.handleRequest(request, response, this.service.getInventory);
  }

  async getOrderById(request, response) {
    await Controller.handleRequest(request, response, this.service.getOrderById);
  }

  async placeOrder(request, response) {
    await Controller.handleRequest(request, response, this.service.placeOrder);
  }

}

module.exports = StoreController;
