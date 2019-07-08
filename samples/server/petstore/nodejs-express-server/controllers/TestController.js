const Service = require('../services/Service');

const testItems = require('../tests/testFiles/testItems.json');

class TestService {
  static testGetController() {
    return new Promise(
      async (resolve, reject) => {
        try {
          resolve(Service.successResponse(
            testItems,
            200,
          ));
        } catch (e) {
          const message = e.getMessage() || 'Could not get items. Server error';
          reject(Service.rejectResponse(message, 500));
        }
      },
    );
  }

  static testDeleteController({ itemId }) {
    return new Promise(
      async (resolve, reject) => {
        try {
          let responseMessage = '';
          const itemToDeleteIndex = testItems.findIndex(item => item.id === itemId);
          if (itemToDeleteIndex > -1) {
            testItems.splice(itemToDeleteIndex, 1);
            responseMessage = `test item id ${itemId} deleted successfully`;
          } else {
            responseMessage = 'test item not found, nothing changed';
          }
          resolve(Service.successResponse(responseMessage, 200));
        } catch (err) {
          const message = err.getMessage() || 'Invalid test item value';
          const code = err.status || 400;
          reject(Service.rejectResponse(message, code));
        }
      },
    );
  }

  static testPostController({ testItem }) {
    return new Promise(
      async (resolve, reject) => {
        try {
          const highestId = testItems[testItems.length - 1].id;
          const newItem = {
            id: highestId + 1,
            name: testItem.name,
            description: testItem.description,
            version: testItem.version,
          };
          testItems.push(newItem);
          resolve(Service.successResponse(newItem));
        } catch (e) {
          reject(Service.rejectResponse(
            e.getMessage() || 'Invalid input',
            e.status || 405,
          ));
        }
      },
    );
  }

  static testFindByIdController({ itemId }) {
    return new Promise(
      async (resolve, reject) => {
        try {
          const itemFound = testItems.find(item => item.id === itemId);
          if (itemFound !== undefined) {
            resolve(Service.successResponse(itemFound, 200));
          } else {
            reject(Service.rejectResponse('item not found', 404));
          }
        } catch (e) {
          reject(Service.rejectResponse(e.getMessage() || 'Invalid ID supplied', 400));
        }
      },
    );
  }

  static testPutController({ itemId, testItem }) {
    return new Promise(
      async (resolve, reject) => {
        try {
          let responseBody;
          const itemToUpdate = testItems.find(item => item.id === itemId);
          if (itemToUpdate !== undefined) {
            itemToUpdate.name = testItem.name || itemToUpdate.name;
            itemToUpdate.description = testItem.description || itemToUpdate.description;
            itemToUpdate.version = testItem.version || itemToUpdate.version;
            responseBody = itemToUpdate;
          } else {
            responseBody = `could not find an item with id ${itemId} to update. Nothing changed`;
          }
          resolve(Service.successResponse(
            responseBody,
            200,
          ));
        } catch (e) {
          reject(Service.rejectResponse(
            e.getMessage || 'Invalid input',
            405,
          ));
        }
      },
    );
  }
}

module.exports = TestService;
