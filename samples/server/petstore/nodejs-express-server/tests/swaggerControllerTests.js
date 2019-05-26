const path = require('path');
const {
  describe, before, after, it,
} = require('mocha');
const chai = require('chai');
const chaiAsPromised = require('chai-as-promised');
const {
  get, post, put, delete: axiosDelete,
} = require('axios');

const logger = require('./logger');
const config = require('./config');
const App = require('../app');
const testItems = require('./testFiles/testItems.json');

// IMPORTANT CHANGE: WORKING ON TEST SWAGGER FILE
config.OPENAPI_YAML = path.join(__dirname, 'testFiles', 'swagger.yaml');

const app = new App(config);
chai.use(chaiAsPromised);
chai.should();

describe('Tests for confirming that the Swagger router works as expected', () => {
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

  it('should test GET request, expecting an array of testItems in response', async () => {
    try {
      const getControllerResponse = await get(`${config.FULL_PATH}/test/controller`);
      getControllerResponse.status.should.equal(200);
      getControllerResponse.data.should.be.an('array');
      getControllerResponse.data.length.should.be.above(0);
    } catch (e) {
      throw e;
    }
  });

  it('should get a single item from the item list using GET request', async () => {
    const item = testItems[0];
    const itemId = item.id;
    const getSingleItemResponse = await get(`${config.FULL_PATH}/test/controller/${itemId}`);
    getSingleItemResponse.status.should.equal(200);
    getSingleItemResponse.data.name.should.equal(item.name);
    getSingleItemResponse.data.description.should.equal(item.description);
    getSingleItemResponse.data.version.should.equal(item.version);
  });

  it('should add a new item to the testItems using POST request', async () => {
    const newItem = {
      name: 'test my new item',
      description: 'a new item that was added using test',
      version: 4,
    };
    try {
      const postResponse = await post(`${config.FULL_PATH}/test/controller`, newItem);
      postResponse.status.should.equal(200);
      postResponse.data.should.be.an('object');
      postResponse.data.should.have.property('id');
      postResponse.data.id.should.be.above(1);
    } catch (e) {
      throw (e);
    }
  });

  it('should update the first item in the test item list using PUT request', async () => {
    const itemToUpdate = testItems[0];
    itemToUpdate.name = 'new name';
    const putResponse = await put(`${config.FULL_PATH}/test/controller/${itemToUpdate.id}`, itemToUpdate);
    putResponse.status.should.equal(200);
    putResponse.data.should.be.an('object');
    putResponse.data.id.should.equal(itemToUpdate.id);
    putResponse.data.name.should.equal(itemToUpdate.name);
  });

  it('should delete the first item in the test item like using DELETE request', async () => {
    const itemToDelete = testItems[0];
    const itemId = itemToDelete.id;
    try {
      const deleteResponse = await axiosDelete(`${config.FULL_PATH}/test/controller/${itemId}`);
      deleteResponse.status.should.equal(200);
      await get(`${config.FULL_PATH}/test/controller/${itemId}`)
        .catch(error => error.response.status.should.equal(404));
    } catch (e) {
      throw (e);
    }
  });
});
