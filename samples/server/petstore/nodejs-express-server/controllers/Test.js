const sendResponse = (request, response) => {
  response.status(200);
  const objectToReturn = {};
  Object.keys(request.swagger.paramValues).forEach((key) => {
    const val = request.swagger.paramValues[key];
    if (!(val instanceof Object)) {
      objectToReturn[key] = request.swagger.paramValues[key];
    } else {
      objectToReturn[key] = val.originalname || val.name;
    }
  });
  response.end(JSON.stringify(objectToReturn));
};

const confirmRoute = (request, response) => {
  sendResponse(request, response);
};

const confirmRoutePost = (request, response) => {
  sendResponse(request, response);
};

const confirmRoutePut = (request, response) => {
  sendResponse(request, response);
};

module.exports = {
  confirmRoute,
  confirmRoutePost,
  confirmRoutePut,
};
