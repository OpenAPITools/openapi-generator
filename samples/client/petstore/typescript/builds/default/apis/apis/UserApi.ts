// TODO: better import syntax?

import { User } from '../';
/**
 * UserApi - interface
 * @export
 * @interface UserApi
 */
export interface UserApiInterface {
    /**
     * This can only be done by the logged in user.
     * @summary Create user
     * @param {User} user Created user object
     * @param {*} [options] Override http request option.
     * @throws {RequiredError}
     * @memberof UserApiInterface
     */
    createUser(user: User, options?: any): Promise<{}>;

    /**
     * 
     * @summary Creates list of users with given input array
     * @param {Array<User>} user List of user object
     * @param {*} [options] Override http request option.
     * @throws {RequiredError}
     * @memberof UserApiInterface
     */
    createUsersWithArrayInput(user: Array<User>, options?: any): Promise<{}>;

    /**
     * 
     * @summary Creates list of users with given input array
     * @param {Array<User>} user List of user object
     * @param {*} [options] Override http request option.
     * @throws {RequiredError}
     * @memberof UserApiInterface
     */
    createUsersWithListInput(user: Array<User>, options?: any): Promise<{}>;

    /**
     * This can only be done by the logged in user.
     * @summary Delete user
     * @param {string} username The name that needs to be deleted
     * @param {*} [options] Override http request option.
     * @throws {RequiredError}
     * @memberof UserApiInterface
     */
    deleteUser(username: string, options?: any): Promise<{}>;

    /**
     * 
     * @summary Get user by user name
     * @param {string} username The name that needs to be fetched. Use user1 for testing.
     * @param {*} [options] Override http request option.
     * @throws {RequiredError}
     * @memberof UserApiInterface
     */
    getUserByName(username: string, options?: any): Promise<User>;

    /**
     * 
     * @summary Logs user into the system
     * @param {string} username The user name for login
     * @param {string} password The password for login in clear text
     * @param {*} [options] Override http request option.
     * @throws {RequiredError}
     * @memberof UserApiInterface
     */
    loginUser(username: string, password: string, options?: any): Promise<string>;

    /**
     * 
     * @summary Logs out current logged in user session
     * @param {*} [options] Override http request option.
     * @throws {RequiredError}
     * @memberof UserApiInterface
     */
    logoutUser(options?: any): Promise<{}>;

    /**
     * This can only be done by the logged in user.
     * @summary Updated user
     * @param {string} username name that need to be deleted
     * @param {User} user Updated user object
     * @param {*} [options] Override http request option.
     * @throws {RequiredError}
     * @memberof UserApiInterface
     */
    updateUser(username: string, user: User, options?: any): Promise<{}>;

}
