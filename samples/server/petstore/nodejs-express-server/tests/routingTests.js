/**
 * The purpose of these tests is to confirm that every path in the openapi spec, if built properly,
 * returns a valid 200, with a simple text response.
 * The codeGen will generate a response string including the name of the operation that was called.
 * These tests confirm that the codeGen worked as expected.
 * Once we start adding our own business logic, these
 * tests will fail. It is recommended to keep these tests updated with the code changes.
 */
const {
  describe, before, after, it,
} = require('mocha');
const assert = require('assert').strict;
const chai = require('chai');
const chaiAsPromised = require('chai-as-promised');
const axios = require('axios');
const yamljs = require('yamljs');
const openApiSampler = require('openapi-sampler');
const jstoxml = require('jstoxml');
const logger = require('./logger');
const config = require('./config');
const ExpressServer = require('../expressServer');

const app = new ExpressServer(config.URL_PORT, config.OPENAPI_YAML);
chai.use(chaiAsPromised);
chai.should();


const pathPrefix = `${config.URL_PATH}:${config.URL_PORT}/api/v2`;
const spec = yamljs.load(config.OPENAPI_YAML);

const parseParameters = (originalPath, schemaParameters) => {
  let path = originalPath;
  const headers = {};
  const queryParams = [];
  schemaParameters.forEach((parameter) => {
    const parameterValue = parameter.example || openApiSampler.sample(parameter.schema);
    switch (parameter.in) {
      case 'header':
        headers[parameter.name] = parameterValue;
        break;
      case 'path':
        path = path.replace(`{${parameter.name}}`, parameterValue);
        break;
      case 'query':
        queryParams.push(`${parameter.name}=${parameterValue}`);
        break;
      default:
        break;
    }
  });
  return { path, headers, queryString: queryParams.join('&') };
};

const buildRequestObject = (pathEndpoint, method, operationObject, requestsArray) => {
  logger.info(`method: ${method}`);
  let headers = {};
  let requestBody = {};
  let queryString = '';
  let path = pathEndpoint;
  if (operationObject.parameters !== undefined) {
    logger.info('this is a request with parameters');
    ({ path, headers, queryString } = parseParameters(pathEndpoint, operationObject.parameters));
    if (queryString.length > 0) {
      path += `?${queryString}`;
    }
    Object.entries(headers).forEach(([headerName, headerValue]) => {
      headers[headerName] = headerValue;
    });
  }
  if (operationObject.requestBody !== undefined) {
    logger.info('This is a request with a body');
    const content = Object.entries(operationObject.requestBody.content);
    content.forEach(([contentType, contentObject]) => {
      requestBody = openApiSampler.sample(contentObject.schema, {}, spec);
      let requestXML;
      if (contentType === 'application/xml') {
        requestXML = jstoxml.toXML(requestBody);
      }
      headers['Content-Type'] = contentType;
      requestsArray.push({
        method,
        path,
        body: requestXML || requestBody,
        headers,
      });
    });
  } else {
    requestsArray.push({
      method,
      path,
      headers,
    });
  }
};

const getApiRequestsData = (apiSchema) => {
  const requestsArray = [];
  Object.entries(apiSchema.paths).forEach(([pathEndpoint, pathObject]) => {
    logger.info(`adding path: ${pathPrefix}${pathEndpoint} to testing array`);
    Object.entries(pathObject).forEach(([operationMethod, operationObject]) => {
      buildRequestObject(pathEndpoint, operationMethod, operationObject, requestsArray);
    });
  });
  return requestsArray;
};

describe('API tests, checking that the codegen generated code that allows all paths specified in schema to work', () => {
  before(async () => {
    try {
      await app.launch();
      logger.info('express server launched\n');
    } catch (error) {
      logger.info(error);
      await app.close();
      throw (error);
    }
  });

  after(async () => {
    await app.close()
      .catch(error => logger.error(error));
    logger.error('express server closed');
  });

  const requestsArray = getApiRequestsData(spec);
  requestsArray.forEach((requestObject) => {
    it(`should run ${requestObject.method.toUpperCase()} request to ${requestObject.path} and return healthy 200`, async () => {
      try {
        const {
          method, path, body, headers,
        } = requestObject;
        const url = `${pathPrefix}${path}`;
        logger.info(`testing ${method.toUpperCase()} call to ${url}. encoding: ${headers['Content-Type']}`);
        const response = await axios({
          method,
          url,
          data: body,
          headers,
        });
        response.should.have.property('status');
        response.status.should.equal(200);
      } catch (e) {
        assert.fail(e.message);
      }
    });
  });
});
