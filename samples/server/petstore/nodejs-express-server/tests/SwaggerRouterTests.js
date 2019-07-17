const path = require('path');
const fs = require('fs');
const { AssertionError } = require('assert');
const {
  describe, before, after, it,
} = require('mocha');
const chai = require('chai');
const chaiAsPromised = require('chai-as-promised');
const { get, post, put } = require('axios');
const FormData = require('form-data');

const logger = require('./logger');
const config = require('./config');
const App = require('../app');

// IMPORTANT CHANGE: WORKING ON TEST SWAGGER FILE

config.OPENAPI_YAML = path.join(__dirname, 'testFiles', 'swagger.yaml');

const app = new App(config);
chai.use(chaiAsPromised);
chai.should();

describe('Tests for confirming that the Openapi router works as expected', () => {
  const pathVarValue = 123;
  const urlPath = `${config.FULL_PATH}/test/${pathVarValue}?queryOptionalNumber=2&queryRequiredString=sss`;
  const headerRequiredArray = [1, 2, 3];
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

  it('Should handle variables sent in the header',
    async () => {
      try {
        const headerOptionalBool = false;
        const headers = { headerOptionalBool, headerRequiredArray };
        const allHeadersResponse = await get(urlPath, { headers });
        allHeadersResponse.status.should.equal(200, 'Expecting a successful allHeadersResponse');
        allHeadersResponse.data.should.have.property('headerOptionalBool').and.equal(headerOptionalBool);
        allHeadersResponse.data.should.have.property('headerRequiredArray');
        allHeadersResponse.data.headerRequiredArray
          .should.have.deep.members(headerRequiredArray);
        const onlyRequiredResponse = await get(urlPath, { headers: { headerRequiredArray } });
        onlyRequiredResponse.status.should.equal(200, 'Expecting a successful onlyRequiredResponse');
        onlyRequiredResponse.data.should.have.property('headerOptionalBool').and.equal(headerOptionalBool);
        onlyRequiredResponse.data.should.have.property('headerRequiredArray');
        onlyRequiredResponse.data.headerRequiredArray.should.have.deep.members(headerRequiredArray);
        await get(urlPath, { headers: { headerOptionalBool } })
          .catch((error => error.response.status.should.equal(400)));
      } catch (error) {
        if (error.response && error.response.data) {
          logger.error(JSON.stringify(error.response.data));
        } else {
          logger.error(error);
        }
        error.should.have.property('response');
        throw new Error(error);
      }
    });

  it('Should handle variables sent in the path',
    async () => {
      try {
        const headers = { headerRequiredArray };
        const correctPathVarResponse = await get(urlPath, { headers });
        correctPathVarResponse.status.should.equal(200);
        correctPathVarResponse.data.should.have.property('pathRequiredNumber').and.equal(pathVarValue);
        const pathVarNotInt = `${config.FULL_PATH}/test/aaa?queryOptionalNumber=2&queryRequiredString=sss`;
        await get(pathVarNotInt, { headers: { headers } })
          .catch((error => error.response.status.should.equal(400)));
      } catch (error) {
        logger.error(error);
        error.should.have.property('response');
        throw new Error(error);
      }
    });

  it('Should handle variables sent in the queryString',
    async () => {
      try {
        const headers = { headerRequiredArray };
        // const allHeadersResponse = await get(urlPath, { headers });
        const queryRequiredString = 'sss';
        const noOptionalQueryUrl = `${config.FULL_PATH}/test/${pathVarValue}/?queryRequiredString=${queryRequiredString}`;
        const noOptionalQueryResponse = await get(noOptionalQueryUrl, { headers });
        noOptionalQueryResponse.status.should.equal(200, 'Expecting a successful allHeadersResponse');
        noOptionalQueryResponse.data.should.have.property('queryRequiredString').and.equal(queryRequiredString);
        noOptionalQueryResponse.data.should.have.property('queryOptionalNumber').and.equal(1);

        const noRequiredQueryUrl = `${config.FULL_PATH}/test/${pathVarValue}`;
        await get(noRequiredQueryUrl, headers)
          .catch((error => error.response.status.should.equal(400)));
      } catch (error) {
        logger.error(error);
        error.should.have.property('response');
        throw new Error(error);
      }
    });

  it('Should handle variables sent in the body',
    async () => {
      try {
        const fullPathURL = `${config.FULL_PATH}/test/`;
        const body = { firstName: 'Foo', lastName: 'Bar' };
        const fullBodyResponse = await post(fullPathURL, body,
          { headers: { headerRequiredArray } });
        fullBodyResponse.status.should.equal(200);
        await post(fullPathURL, { firstName: 'foo' },
          { headers: { headerRequiredArray } })
          .then(response => response.status.should.equal(200));
        await post(fullPathURL, { lastName: 'bar' },
          { headers: { headerRequiredArray } })
          .catch(error => error.response.status.should.equal(400));
      } catch (error) {
        error.should.have.property('response');
        console.error(JSON.stringify(error.response.data));
      }
    });

  it('Should handle variables sent in the formData',
    async () => {
      try {
        const fullPathURL = `${config.FULL_PATH}/test/123`;
        const formData = new FormData();
        const person = {
          firstName: 'Foo',
          lastName: 'Bar',
          image: fs.createReadStream(path.join(__dirname, 'testFiles', 'pet.json')),
        };
        Object.keys(person).forEach(key => formData.append(key, person[key]));
        const formResponse = await put(fullPathURL, formData,
          {
            headers: formData.getHeaders(),
          });
        formResponse.status.should.equal(200);
        Object.keys(person).forEach((nameKey) => {
          formResponse.data.should.have.property(nameKey);
          if (person[nameKey] instanceof String) {
            formResponse.data[nameKey].should.equal(person[nameKey]);
          }
        });
      } catch (error) {
        if (error instanceof AssertionError) {
          throw error;
        }
        error.should.have.property('response');
        console.error(JSON.stringify(error.response.data));
        throw new Error(error);
      }
    });
});
