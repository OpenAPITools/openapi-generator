/* eslint-disable no-unused-vars */
const Service = require('./Service');

/**
* Create user
* This can only be done by the logged in user.
*
* body User Created user object
* no response value expected for this operation
* */
const createUser = ({ body }) => new Promise(
  async (resolve, reject) => {
    try {
      resolve(Service.successResponse({
        body,
      }));
    } catch (e) {
      reject(Service.rejectResponse(
        e.message || 'Invalid input',
        e.status || 405,
      ));
    }
  },
);
/**
* Creates list of users with given input array
*
* body List List of user object
* no response value expected for this operation
* */
const createUsersWithArrayInput = ({ body }) => new Promise(
  async (resolve, reject) => {
    try {
      resolve(Service.successResponse({
        body,
      }));
    } catch (e) {
      reject(Service.rejectResponse(
        e.message || 'Invalid input',
        e.status || 405,
      ));
    }
  },
);
/**
* Creates list of users with given input array
*
* body List List of user object
* no response value expected for this operation
* */
const createUsersWithListInput = ({ body }) => new Promise(
  async (resolve, reject) => {
    try {
      resolve(Service.successResponse({
        body,
      }));
    } catch (e) {
      reject(Service.rejectResponse(
        e.message || 'Invalid input',
        e.status || 405,
      ));
    }
  },
);
/**
* Delete user
* This can only be done by the logged in user.
*
* username String The name that needs to be deleted
* no response value expected for this operation
* */
const deleteUser = ({ username }) => new Promise(
  async (resolve, reject) => {
    try {
      resolve(Service.successResponse({
        username,
      }));
    } catch (e) {
      reject(Service.rejectResponse(
        e.message || 'Invalid input',
        e.status || 405,
      ));
    }
  },
);
/**
* Get user by user name
*
* username String The name that needs to be fetched. Use user1 for testing.
* returns User
* */
const getUserByName = ({ username }) => new Promise(
  async (resolve, reject) => {
    try {
      resolve(Service.successResponse({
        username,
      }));
    } catch (e) {
      reject(Service.rejectResponse(
        e.message || 'Invalid input',
        e.status || 405,
      ));
    }
  },
);
/**
* Logs user into the system
*
* username String The user name for login
* password String The password for login in clear text
* returns String
* */
const loginUser = ({ username, password }) => new Promise(
  async (resolve, reject) => {
    try {
      resolve(Service.successResponse({
        username,
        password,
      }));
    } catch (e) {
      reject(Service.rejectResponse(
        e.message || 'Invalid input',
        e.status || 405,
      ));
    }
  },
);
/**
* Logs out current logged in user session
*
* no response value expected for this operation
* */
const logoutUser = () => new Promise(
  async (resolve, reject) => {
    try {
      resolve(Service.successResponse({
      }));
    } catch (e) {
      reject(Service.rejectResponse(
        e.message || 'Invalid input',
        e.status || 405,
      ));
    }
  },
);
/**
* Updated user
* This can only be done by the logged in user.
*
* username String name that need to be deleted
* body User Updated user object
* no response value expected for this operation
* */
const updateUser = ({ username, body }) => new Promise(
  async (resolve, reject) => {
    try {
      resolve(Service.successResponse({
        username,
        body,
      }));
    } catch (e) {
      reject(Service.rejectResponse(
        e.message || 'Invalid input',
        e.status || 405,
      ));
    }
  },
);

module.exports = {
  createUser,
  createUsersWithArrayInput,
  createUsersWithListInput,
  deleteUser,
  getUserByName,
  loginUser,
  logoutUser,
  updateUser,
};
