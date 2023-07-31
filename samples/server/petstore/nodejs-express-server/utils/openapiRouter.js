const logger = require('../logger');
const controllers = require('../controllers');
const Services = require('../services');

function handleError(err, request, response, next) {
  logger.error(err);
  const code = err.code || 400;
  response.status(code);
  response.error = err;
  next(JSON.stringify({
    code,
    error: err,
  }));
}

/**
 * The purpose of this route is to collect the request variables as defined in the
 * OpenAPI document and pass them to the handling controller as another Express
 * middleware. All parameters are collected in the request.swagger.values key-value object
 *
 * The assumption is that security handlers have already verified and allowed access
 * to this path. If the business-logic of a particular path is dependent on authentication
 * parameters (e.g. scope checking) - it is recommended to define the authentication header
 * as one of the parameters expected in the OpenAPI/Swagger document.
 *
 *  Requests made to paths that are not in the OpenAPI scope
 *  are passed on to the next middleware handler.
 * @returns {Function}
 */
function openApiRouter() {
  return async (request, response, next) => {
    try {
      /**
       * This middleware runs after a previous process have applied an openapi object
       * to the request.
       * If none was applied This is because the path requested is not in the schema.
       * If there's no openapi object, we have nothing to do, and pass on to next middleware.
       */
      if (request.openapi === undefined
          || request.openapi.schema === undefined
      ) {
        next();
        return;
      }
      // request.swagger.paramValues = {};
      // request.swagger.params.forEach((param) => {
      //   request.swagger.paramValues[param.name] = getValueFromRequest(request, param);
      // });
      const controllerName = request.openapi.schema['x-openapi-router-controller'];
      const serviceName = request.openapi.schema['x-openapi-router-service'];
      if (!controllers[controllerName] || controllers[controllerName] === undefined) {
        handleError(`request sent to controller '${controllerName}' which has not been defined`,
          request, response, next);
      } else {
        const apiController = new controllers[controllerName](Services[serviceName]);
        const controllerOperation = request.openapi.schema.operationId;
        await apiController[controllerOperation](request, response, next);
      }
    } catch (error) {
      console.error(error);
      const err = { code: 500, error: error.message };
      handleError(err, request, response, next);
    }
  };
}

module.exports = openApiRouter;
