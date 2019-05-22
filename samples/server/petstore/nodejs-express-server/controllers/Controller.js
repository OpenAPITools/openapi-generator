class Controller {
  static sendResponse(response, payload) {
    response.status(payload.code);
    if (payload.payload instanceof Object) {
      response.json(payload.payload);
    } else {
      response.end(payload.payload);
    }
  }

  static sendError(response, error) {
    response.status(error.code);
    if (error.error instanceof Object) {
      response.json(error.error);
    } else {
      response.end(error.error);
    }
  }

  static async handleRequest(request, response, serviceOperation) {
    try {
      const serviceResponse = await serviceOperation(request.swagger.paramValues);
      Controller.sendResponse(response, serviceResponse);
    } catch (error) {
      Controller.sendError(response, error);
    }
  }
}

module.exports = Controller;
