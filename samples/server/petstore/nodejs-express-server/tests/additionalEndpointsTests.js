const {
  describe, before, after, it,
} = require('mocha');
const assert = require('assert').strict;
const chai = require('chai');
const chaiAsPromised = require('chai-as-promised');
const axios = require('axios');
const logger = require('./logger');
const config = require('./config');
const ExpressServer = require('../expressServer');

const app = new ExpressServer(config.URL_PORT, config.OPENAPI_YAML);
chai.use(chaiAsPromised);
chai.should();

describe('Test endpoints that are not part of the openapi.yaml.', () => {
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


  it('should confirm that requesting the openapi.yaml returns a successful 200 response', async () => {
    const pathToCall = `${config.URL_PATH}:${config.URL_PORT}/spec/openapi.yaml`;
    try {
      const openapiResponse = await axios.get(pathToCall);
      openapiResponse.should.have.property('status');
      openapiResponse.status.should.equal(200);
    } catch (e) {
      logger.error(`failed to call ${pathToCall}`);
      assert.fail(`Failed to call openapi.yaml - ${e.message}`);
    }
  });
});
