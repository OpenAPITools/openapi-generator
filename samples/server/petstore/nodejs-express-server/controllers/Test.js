const Controller = require('./Controller');

class Test {
  constructor(Service) {
    this.service = Service;
  }

  sendResponse(request, response) {
    response.status(200);
    const objectToReturn = {};
    Object.keys(request.swagger.paramValues).forEach((key) => {
      const val = request.swagger.paramValues[key];
      if (val instanceof Object) {
        objectToReturn[key] = val.originalname || val.name || val;
      } else {
        objectToReturn[key] = request.swagger.paramValues[key];
      }
    });
    response.json(objectToReturn);
  }

  confirmRouteGetSingle(request, response) {
    this.sendResponse(request, response);
  }

  confirmRouteGetMany(request, response) {
    this.sendResponse(request, response);
  }

  confirmRoutePost(request, response) {
    this.sendResponse(request, response);
  }

  confirmRoutePut(request, response) {
    this.sendResponse(request, response);
  }

  async testGetController(request, response) {
    await Controller.handleRequest(request, response, this.service.testGetController);
  }

  async testPostController(request, response) {
    await Controller.handleRequest(request, response, this.service.testPostController);
  }

  async testPutController(request, response) {
    await Controller.handleRequest(request, response, this.service.testPutController);
  }

  async testDeleteController(request, response) {
    await Controller.handleRequest(request, response, this.service.testDeleteController);
  }

  async testFindByIdController(request, response) {
    await Controller.handleRequest(request, response, this.service.testFindByIdController);
  }
}

module.exports = Test;
