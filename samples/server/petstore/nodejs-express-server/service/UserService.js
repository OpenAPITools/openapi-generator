const Service = require('./Service');

class UserService {
  static loginUser({ username, password }) {
    return new Promise(
      async (resolve) => {
        try {
          if (username !== undefined && password !== undefined) {
            resolve(Service.successResponse({ string: 'String' }));
          } else {
            resolve(Service.rejectResponse('Wrong username/password', 400));
          }
        } catch (e) {
          resolve(Service.rejectResponse(e.message));
        }
      },
    );
  }

  static logoutUser() {
    return new Promise(
      (resolve) => {
        try {
          resolve(Service.successResponse('logout user'));
        } catch (e) {
          resolve(Service.rejectResponse(e.message));
        }
      },
    );
  }

  static deleteUser({ username }) {
    return new Promise(
      (resolve) => {
        try {
          if (username !== undefined) {
            resolve(Service.successResponse('deleteUser'));
          } else {
            resolve(Service.rejectResponse('Invalid username supplied', 400));
          }
        } catch (e) {
          resolve(Service.rejectResponse(e.message));
        }
      },
    );
  }

  static getUserByName({ username }) {
    return new Promise(
      (resolve) => {
        try {
          resolve(Service.successResponse({
            id: 1,
            username,
            firstName: 'firstName',
            lastName: 'lastName',
            email: 'email',
            phone: '213-456-7890',
            userStatus: 1,
          }));
        } catch (e) {
          resolve(Service.rejectResponse(e.message));
        }
      },
    );
  }

  static updateUser({ username, user }) {
    return new Promise(
      (resolve) => {
        try {
          if (user.username === username) {
            resolve(Service.successResponse(user));
          } else {
            resolve(Service.successResponse({}));
          }
        } catch (e) {
          resolve(Service.rejectResponse(e.message));
        }
      },
    );
  }
}

module.exports = UserService;
