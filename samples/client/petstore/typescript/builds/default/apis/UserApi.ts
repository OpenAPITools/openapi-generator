// TODO: better import syntax?
import { BaseApiRequestFactory } from './baseapi';
import { RequestContext } from '../http/http';
import { User } from '../models/User';




/**
 * UserApi - interface
 * @export
 * @interface UserApi
 */
export class UserApiRequestFactory {

    /**
     * This can only be done by the logged in user.
     * @summary Create user
     * @param {User} user Created user object
     * @param {*} [options] Override http request option.
     * @throws {RequiredError}
     * @memberof UserApiInterface
     */
    public createUser(user: User, options?: any): RequestContext {
    	
    	
    	return null;
    }
			
    /**
     * 
     * @summary Creates list of users with given input array
     * @param {Array<User>} user List of user object
     * @param {*} [options] Override http request option.
     * @throws {RequiredError}
     * @memberof UserApiInterface
     */
    public createUsersWithArrayInput(user: Array<User>, options?: any): RequestContext {
    	
    	
    	return null;
    }
			
    /**
     * 
     * @summary Creates list of users with given input array
     * @param {Array<User>} user List of user object
     * @param {*} [options] Override http request option.
     * @throws {RequiredError}
     * @memberof UserApiInterface
     */
    public createUsersWithListInput(user: Array<User>, options?: any): RequestContext {
    	
    	
    	return null;
    }
			
    /**
     * This can only be done by the logged in user.
     * @summary Delete user
     * @param {string} username The name that needs to be deleted
     * @param {*} [options] Override http request option.
     * @throws {RequiredError}
     * @memberof UserApiInterface
     */
    public deleteUser(username: string, options?: any): RequestContext {
    	
    	
    	return null;
    }
			
    /**
     * 
     * @summary Get user by user name
     * @param {string} username The name that needs to be fetched. Use user1 for testing.
     * @param {*} [options] Override http request option.
     * @throws {RequiredError}
     * @memberof UserApiInterface
     */
    public getUserByName(username: string, options?: any): RequestContext {
    	
    	
    	return null;
    }
			
    /**
     * 
     * @summary Logs user into the system
     * @param {string} username The user name for login
     * @param {string} password The password for login in clear text
     * @param {*} [options] Override http request option.
     * @throws {RequiredError}
     * @memberof UserApiInterface
     */
    public loginUser(username: string, password: string, options?: any): RequestContext {
    	
    	
    	return null;
    }
			
    /**
     * 
     * @summary Logs out current logged in user session
     * @param {*} [options] Override http request option.
     * @throws {RequiredError}
     * @memberof UserApiInterface
     */
    public logoutUser(options?: any): RequestContext {
    	
    	
    	return null;
    }
			
    /**
     * This can only be done by the logged in user.
     * @summary Updated user
     * @param {string} username name that need to be deleted
     * @param {User} user Updated user object
     * @param {*} [options] Override http request option.
     * @throws {RequiredError}
     * @memberof UserApiInterface
     */
    public updateUser(username: string, user: User, options?: any): RequestContext {
    	
    	
    	return null;
    }
			
}
