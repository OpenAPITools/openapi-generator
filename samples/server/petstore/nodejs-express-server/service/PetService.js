const Service = require('./Service');

class PetService {
  static deletePet({ apiKey, petId }) {
    return new Promise(
      async (resolve, reject) => {
        try {
          const responseMessage = `success: apiKey ${apiKey}, petId: ${petId}`;
          resolve(Service.successResponse(responseMessage, 200));
        } catch (err) {
          const message = err.getMessage() || 'Invalid pet value';
          const code = err.status || 400;
          reject(Service.rejectResponse(message, code));
        }
      },
    );
  }

  static addPet({ body }) {
    return new Promise(
      async (resolve, reject) => {
        try {
          resolve(Service.successResponse(body));
        } catch (e) {
          reject(Service.rejectResponse(
            e.getMessage() || 'Invalid input',
            e.status || 405,
          ));
        }
      },
    );
  }

  static findPetsByStatus({ status }) {
    return new Promise(
      async (resolve, reject) => {
        try {
          const examples = {};
          examples['application/json'] = [
            {
              photoUrls: ['photoUrls', 'photoUrls'],
              name: 'doggie',
              id: 0,
              category: {
                name: 'name',
                id: 6,
              },
              tags: [{
                name: 'name',
                id: 1,
              }, {
                name: 'name',
                id: 1,
              }],
              status: 'available',
            },
          ];
          resolve(Service.successResponse(
            Object.keys(examples)[0] || {},
            200,
          ));
        } catch (e) {
          reject(
            Service.rejectResponse(
              e.getMessage() || 'Invalid status value',
              400,
            ),
          );
        }
      },
    );
  }

  static findPetsByTags({ tags }) {
    return new Promise(
      async (resolve, reject) => {
        try {
          const examples = {};
          examples['application/json'] = [
            {
              photoUrls: ['photoUrls', 'photoUrls'],
              name: 'doggie',
              id: 0,
              category: {
                name: 'name',
                id: 6,
              },
              tags: [{
                name: 'name',
                id: 1,
              }, {
                name: 'name',
                id: 1,
              }],
              status: 'available',
            },
          ];
          resolve(Service.successResponse(
            Object.keys(examples)[0] || {},
            200,
          ));
        } catch (e) {
          reject(
            Service.rejectResponse(
              e.getMessage() || 'Invalid tag value',
              400,
            ),
          );
        }
      },
    );
  }

  static getPetById({ petId }) {
    return new Promise(
      async (resolve, reject) => {
        try {
          const examples = {};
          examples['application/json'] = {
            photoUrls: ['photoUrls', 'photoUrls'],
            name: 'doggie',
            id: 0,
            category: {
              name: 'name',
              id: 6,
            },
            tags: [{
              name: 'name',
              id: 1,
            }, {
              name: 'name',
              id: 1,
            }],
            status: 'available',
          };
          if (Object.keys(examples).length > 0) {
            resolve(Service.successResponse(examples[Object.keys(examples)[0]], 200));
          } else {
            reject(Service.rejectResponse('Pet not found', 404));
          }
        } catch (e) {
          reject(Service.rejectResponse(e.getMessage() || 'Invalid ID supplied', 400));
        }
      },
    );
  }

  static updatePet({ petId, name, status }) {
    return new Promise(
      async (resolve, reject) => {
        try {
          resolve(Service.successResponse(
            `Success. petId: ${petId}, name: ${name}, status: ${status}`,
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

  static uploadFile({ petId, additionalMetadata, file }) {
    return new Promise(
      async (resolve, reject) => {
        try {
          const examples = {};
          examples['application/json'] = {
            code: 0,
            type: 'type',
            message: 'message',
          };
          resolve(Service.successResponse(
            Object.keys((examples)[0]) || {},
            200,
          ));
        } catch (e) {
          reject(Service.rejectResponse(
            e.getMessage() || '',
          ));
        }
      },
    );
  }
}

module.exports = PetService;
