/* eslint-disable no-unused-vars */
const Service = require('./Service');

class PetService {
  /**
   *  Add a new pet to the store
   * @param body
   * @returns {Promise<any>}
   */
  static addPet({ body }) {
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

  static updatePet({ body }) {
    return new Promise(
      async (resolve) => {
        try {
          resolve(Service.successResponse(body));
        } catch (e) {
          resolve(Service.rejectResponse(
            'Invalid ID supplied',
            400,
          ));
        }
      },
    );
  }

  /**
   * Multiple status values can be provided with comma separated strings
   * @param status
   * @returns {Promise<any>}
   */
  static findPetsByStatus({ status }) {
    return new Promise(
      async (resolve) => {
        try {
          resolve([
            {
              photoUrls: ['photoUrls_1', 'photoUrls_2'],
              name: 'dog_0',
              id: 0,
              category: {
                name: 'category',
                id: 1,
              },
              tags: [{
                name: 'tag_1',
                id: 1,
              }, {
                name: 'tag_2',
                id: 2,
              }],
              status: 'available',
            },
            {
              photoUrls: ['photoUrls_1', 'photoUrls_2'],
              name: 'dog_1',
              id: 1,
              category: {
                name: 'category',
                id: 1,
              },
              tags: [{
                name: 'tags_1',
                id: 1,
              }, {
                name: 'tags_2',
                id: 2,
              }],
              status: 'available',
            }], 200);
        } catch (e) {
          resolve(
            Service.rejectResponse(
              e.getMessage() || 'Invalid status value',
              400,
            ),
          );
        }
      },
    );
  }

  /**
   * Multiple tags can be provided with comma separated strings. Use
   tag1, tag2, tag3 for testing.
   * @param tags
   * @returns {Promise<any>}
   */
  static findPetsByTags({ tags }) {
    return new Promise(
      async (resolve) => {
        try {
          resolve(Service.successResponse(
            [
              {
                photoUrls: ['photoUrls_1', 'photoUrls_2'],
                name: 'dog_0',
                id: 0,
                category: {
                  name: 'category',
                  id: 1,
                },
                tags: [{
                  name: 'tag_1',
                  id: 1,
                }, {
                  name: 'tag_2',
                  id: 2,
                }],
                status: 'available',
              },
              {
                photoUrls: ['photoUrls_1', 'photoUrls_2'],
                name: 'dog_1',
                id: 1,
                category: {
                  name: 'category',
                  id: 1,
                },
                tags: [{
                  name: 'tags_1',
                  id: 1,
                }, {
                  name: 'tags_2',
                  id: 2,
                }],
                status: 'available',
              },
            ], 200,
          ));
        } catch (e) {
          resolve(
            Service.rejectResponse(
              'Invalid tag value',
              400,
            ),
          );
        }
      },
    );
  }

  static deletePet({ apiKey, petId }) {
    return new Promise(
      async (resolve) => {
        try {
          resolve(Service.successResponse(`success. apiKey ${apiKey}, petId: ${petId}`));
        } catch (err) {
          resolve(Service.rejectResponse('Invalid pet value', 400));
        }
      },
    );
  }


  /**
   * Returns a single pet
   * @param petId
   * @returns {Promise<any>}
   */
  static getPetById({ petId }) {
    return new Promise(
      async (resolve) => {
        try {
          resolve({
            photoUrls: ['photoUrls_1', 'photoUrls_2'],
            name: 'dog_0',
            id: 0,
            category: {
              name: 'category',
              id: 1,
            },
            tags: [{
              name: 'tag_1',
              id: 1,
            }, {
              name: 'tag_2',
              id: 2,
            }],
            status: 'available',
          }, 200);
        } catch (e) {
          resolve(Service.rejectResponse(e.getMessage() || 'Invalid ID supplied', 400));
        }
      },
    );
  }

  /**
   *
   * @param petId -- ID of pet that needs to be updated -- required
   * @param body
   * @returns {Promise<any>}
   */
  static updatePetWithForm({ petId, body }) {
    return new Promise(
      (resolve) => {
        try {
          resolve(Service.successResponse(''));
        } catch (e) {
          resolve(Service.rejectResponse('Invalid input', 405));
        }
      },
    );
  }

  /**
   *
   * @param petId --  ID of pet to update
   * @param body -- additionalMetadata, file
   * @returns {Promise<any>}
   */
  static uploadFile({ petId, body }) {
    return new Promise(
      async (resolve) => {
        try {
          resolve(Service.successResponse(
            {
              code: 0,
              type: 'type',
              message: 'message',
            },
            200,
          ));
        } catch (e) {
          resolve(Service.rejectResponse(''));
        }
      },
    );
  }
}

module.exports = PetService;
