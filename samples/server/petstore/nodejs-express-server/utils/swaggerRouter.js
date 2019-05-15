const { camelCase, upperFirst } = require('lodash');

const logger = require('../logger');
const controllers = require('../controllers');

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
      valueToReturn = request.headers[paramObject.name];
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
        const swaggerParams = request.swagger.params.map(
          param => getValueFromRequest(request, param),
        );
        const controllerName = upperFirst(camelCase(request.swagger.operation.tags[0]));
        const controller = controllers[controllerName];
        const controllerOperation = request.swagger.operation.operationId;
        const controllerResponse = await controller[controllerOperation](...swaggerParams);
        response.status(200);
        response.send(JSON.stringify(controllerResponse));
      }
    } catch (error) {
      console.error(error);
      const err = { code: 500, error: error.message };
      handleError(err, request, response, next);
    }
  };
}

module.exports = swaggerRouter;
