/* eslint-disable no-unused-vars */
const Service = require('./Service');

class PetService {

  /**
   * Add a new pet to the store
   *
   * body Pet Pet object that needs to be added to the store
   * no response value expected for this operation
   **/
  static addPet({ body }) {
    return new Promise(
      async (resolve) => {
        try {
          resolve(Service.successResponse(''));
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
   * Deletes a pet
   *
   * petId Long Pet id to delete
   * apiUnderscorekey String  (optional)
   * no response value expected for this operation
   **/
  static deletePet({ petId, apiUnderscorekey }) {
    return new Promise(
      async (resolve) => {
        try {
          resolve(Service.successResponse(''));
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
   * Finds Pets by status
   * Multiple status values can be provided with comma separated strings
   *
   * status List Status values that need to be considered for filter
   * returns List
   **/
  static findPetsByStatus({ status }) {
    return new Promise(
      async (resolve) => {
        try {
          resolve(Service.successResponse(''));
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
   * Finds Pets by tags
   * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
   *
   * tags List Tags to filter by
   * returns List
   **/
  static findPetsByTags({ tags }) {
    return new Promise(
      async (resolve) => {
        try {
          resolve(Service.successResponse(''));
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
   * Find pet by ID
   * Returns a single pet
   *
   * petId Long ID of pet to return
   * returns Pet
   **/
  static getPetById({ petId }) {
    return new Promise(
      async (resolve) => {
        try {
          resolve(Service.successResponse(''));
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
   * Update an existing pet
   *
   * body Pet Pet object that needs to be added to the store
   * no response value expected for this operation
   **/
  static updatePet({ body }) {
    return new Promise(
      async (resolve) => {
        try {
          resolve(Service.successResponse(''));
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
   * Updates a pet in the store with form data
   *
   * petId Long ID of pet that needs to be updated
   * name String Updated name of the pet (optional)
   * status String Updated status of the pet (optional)
   * no response value expected for this operation
   **/
  static updatePetWithForm({ petId, name, status }) {
    return new Promise(
      async (resolve) => {
        try {
          resolve(Service.successResponse(''));
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
   * uploads an image
   *
   * petId Long ID of pet to update
   * additionalMetadata String Additional data to pass to server (optional)
   * file File file to upload (optional)
   * returns ApiResponse
   **/
  static uploadFile({ petId, additionalMetadata, file }) {
    return new Promise(
      async (resolve) => {
        try {
          resolve(Service.successResponse(''));
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

module.exports = PetService;
