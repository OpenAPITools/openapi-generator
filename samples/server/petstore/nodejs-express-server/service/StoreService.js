/* eslint-disable no-unused-vars */
const Service = require('./Service');

class StoreControllerService {

  /**
   * Delete purchase order by ID
   * For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors
   *
   * orderId String ID of the order that needs to be deleted
   * no response value expected for this operation
   **/
  static.deleteOrder = function({ orderId }) {
    return new Promise(
      async (resolve) => {
        try {
          resolve(Service.successResponse(body));
        } catch (e) {
          resolve(Service.rejectResponse(
            e.message || 'Invalid input',
            e.status || 405,
          ));
        }
      },
    );
  }

  /**
   * Returns pet inventories by status
   * Returns a map of status codes to quantities
   *
   * returns Map
   **/
  static.getInventory = function({  }) {
    return new Promise(
      async (resolve) => {
        try {
          resolve(Service.successResponse(body));
        } catch (e) {
          resolve(Service.rejectResponse(
            e.message || 'Invalid input',
            e.status || 405,
          ));
        }
      },
    );
  }

  /**
   * Find purchase order by ID
   * For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions
   *
   * orderId Long ID of pet that needs to be fetched
   * returns Order
   **/
  static.getOrderById = function({ orderId }) {
    return new Promise(
      async (resolve) => {
        try {
          resolve(Service.successResponse(body));
        } catch (e) {
          resolve(Service.rejectResponse(
            e.message || 'Invalid input',
            e.status || 405,
          ));
        }
      },
    );
  }

  /**
   * Place an order for a pet
   *
   * body Order order placed for purchasing the pet
   * returns Order
   **/
  static.placeOrder = function({ body }) {
    return new Promise(
      async (resolve) => {
        try {
          resolve(Service.successResponse(body));
        } catch (e) {
          resolve(Service.rejectResponse(
            e.message || 'Invalid input',
            e.status || 405,
          ));
        }
      },
    );
  }

}

module.exports = StoreControllerService;
