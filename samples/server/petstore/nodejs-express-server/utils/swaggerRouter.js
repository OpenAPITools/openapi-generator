const logger = require('../logger');
const controllers = require('../controllers');
const Services = require('../service');

function getValueFromRequest(request, paramObject) {
  let valueToReturn;
  switch (paramObject.in) {
    default:
      break;
    case 'path':
      valueToReturn = request.pathParams[paramObject.name];
      break;
    case 'body':
      valueToReturn = request.body;
      break;
    case 'query':
      valueToReturn = request.query[paramObject.name];
      break;
    case 'header':
      /**
       * A different middleware converted header variable names to lowercase.
       * Seems like Apache Tomcat does the same and http2 requires lower-case.
       * We keep the original casing.
       */
      valueToReturn = request.headers[paramObject.name.toLowerCase()];
      break;
    case 'formData':
      if (paramObject.type === 'string') {
        valueToReturn = request.body[paramObject.name];
        break;
      }
      if (paramObject.type === 'file') {
        valueToReturn = request.files[paramObject.name];
      }
  }
  return valueToReturn;
}

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
 * Swagger/OpenAPI document and pass them to the handling controller as another Express
 * middleware. All parameters are collected in the requet.swagger.values key-value object
 *
 * The assumption is that security handlers have already verified and allowed access
 * to this path. If the business-logic of a particular path is dependant on authentication
 * parameters (e.g. scope checking) - it is recommended to define the authentication header
 * as one of the parameters expected in the OpenAPI/Swagger document.
 *
 *  Requests made to paths that are not in the OpernAPI/Swagger scope
 *  are passed on to the next middleware handler.
 * @returns {Function}
 */
function swaggerRouter() {
  return async (request, response, next) => {
    try {
      /**
       * Previous process should have applied a swagger object to the request. If none was applied
       * This is because the path requested is not in the schema. Nothing to do here.
       */
      if (request.swagger === undefined) {
        next();
        return;
      }
      if (!request.swagger.operation
          || request.swagger.operation === undefined
          || request.swagger.operation === null) {
        next();
      } else {
        request.swagger.paramValues = {};
        request.swagger.params.forEach((param) => {
          request.swagger.paramValues[param.name] = getValueFromRequest(request, param);
        });
        const controllerName = request.swagger.operation['x-swagger-router-controller'];
        const serviceName = `${controllerName}Service`;
        if (!controllers[controllerName] || controllers[controllerName] === undefined) {
          handleError(`request sent to controller '${controllerName}' which has not been defined`,
            request, response, next);
        } else {
          const apiController = new controllers[controllerName](Services[serviceName]);
          const controllerOperation = request.swagger.operation.operationId;
          await apiController[controllerOperation](request, response, next);
        }
      }
    } catch (error) {
      console.error(error);
      const err = { code: 500, error: error.message };
      handleError(err, request, response, next);
    }
  };
}

module.exports = swaggerRouter;
