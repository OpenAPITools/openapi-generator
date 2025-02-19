const {
  describe, before, after, it,
} = require('mocha');
const chai = require('chai');
const chaiAsPromised = require('chai-as-promised');
const { get } = require('axios');

const logger = require('./logger');
const config = require('./config');
const ExpressServer = require('../expressServer');

const app = new ExpressServer(config.URL_PORT, config.OPENAPI_YAML);
chai.use(chaiAsPromised);
chai.should();

describe('Server tests, checking launch, terminate, and various error messages', () => {
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

  it('should launch express server successfully', async () => {
    const indexResponse = await get(`${config.URL_PATH}:${config.URL_PORT}/`);
    indexResponse.status.should.equal(200, 'Expecting a call to root directory of server to return 200 code');
  });

  it('should fail with a 404 on non-existing page', async () => {
    get(`${config.FULL_PATH}/someRandomPage`)
      .then(response => response.status.should.equal(404, 'expecting a 404 on a non-existing page request'))
      .catch((responseError) => {
        responseError.response.status.should.not.equal(undefined);
        responseError.response.status.should.equal(404, 'expecting to receive a 404 on requesting a non-existing page');
      });
  });

  it('should load api-doc', async () => {
    try {
      const response = await get(`http://localhost:${config.URL_PORT}/api-docs`);
      response.status.should.equal(200, 'Expecting 200');
    } catch (e) {
      console.log(e.message);
      throw e;
    }
  });
});
