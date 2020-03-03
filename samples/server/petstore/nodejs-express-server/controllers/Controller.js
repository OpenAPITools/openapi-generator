const fs = require('fs');
const camelcase = require('camelcase');
const logger = require('../logger');

const sendResponse = (response, payload) => {
  /**
   * The default response-code is 200. We want to allow to change that. in That case,
   * payload will be an object consisting of a code and a payload. If not customized
   * send 200 and the payload as received in this method.
   */
  if (payload.code) {
    response.status(payload.code);
  } else {
    response.status(200);
  }
  response.status(payload.code || 200);
  const responsePayload = payload.payload !== undefined ? payload.payload : payload;
  if (typeof responsePayload === 'object' && typeof responsePayload !== 'function' && responsePayload !== null) {
    response.json(responsePayload);
  } else {
    response.end(responsePayload);
  }
};

const sendError = (response, error) => {
  response.status(error.code || 500);
  if (error.error instanceof Object) {
    response.json(error.error);
  } else {
    response.end(error.message);
  }
};

const getBodyObjectName = (request) => {
  if (request.openapi.schema.requestBody) {
    const { requestBody } = request.openapi.schema;
    if (requestBody.$ref) {
      return camelcase(requestBody.$ref.substr(requestBody.$ref.lastIndexOf('/') + 1));
    }
    if (requestBody.content['application/json'] && requestBody.content['application/json'].schema) {
      const bodyRef = requestBody.content['application/json'].schema.$ref;
      if (bodyRef === undefined || bodyRef.length === 0) {
        return 'body';
      }
      return camelcase(bodyRef.substr(bodyRef.lastIndexOf('/') + 1));
    }
  }
  return '';
};

const collectRequestParams = async request => new Promise(
  async (resolve, reject) => {
    try {
      const requestParams = {};
      if (request.files) {
        request.files.forEach((f) => {
          const savedFile = f.path;
          const newPath = f.path.replace(f.filename, f.originalname);
          fs.renameSync(savedFile, newPath);
          request.body[f.fieldname] = newPath;
        });
      }
      if (request.openapi.schema.requestBody) {
        const bodyObjectName = getBodyObjectName(request);
        if (bodyObjectName !== '') {
          requestParams[bodyObjectName] = request.body;
        }
      }
      request.openapi.schema.parameters.forEach((param) => {
        if (param.in === 'path') {
          requestParams[param.name] = request.openapi.pathParams[param.name];
        } else if (param.in === 'query') {
          requestParams[param.name] = request.query[param.name];
        } else if (param.in === 'header') {
          requestParams[param.name] = request.headers[param.name];
        }
      });
      resolve(requestParams);
    } catch (e) {
      reject(e);
    }
  },
);

const handleRequest = async (request, response, serviceOperation) => {
  try {
    logger.debug('Handling request', request.path);
    const requestParams = await collectRequestParams(request)
      .catch(e => sendError(response, JSON.stringify(e)));
    const serviceResponse = await serviceOperation(requestParams);
    sendResponse(response, serviceResponse);
  } catch (error) {
    sendError(response, error.message || error);
  }
};

module.exports = {
  handleRequest,
};
