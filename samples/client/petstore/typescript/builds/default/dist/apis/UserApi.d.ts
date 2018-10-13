import { BaseAPIRequestFactory } from './baseapi';
import { RequestContext, ResponseContext } from '../http/http';
import { User } from '../models/User';
export declare class UserApiRequestFactory extends BaseAPIRequestFactory {
    createUser(user: User, options?: any): RequestContext;
    createUsersWithArrayInput(user: Array<User>, options?: any): RequestContext;
    createUsersWithListInput(user: Array<User>, options?: any): RequestContext;
    deleteUser(username: string, options?: any): RequestContext;
    getUserByName(username: string, options?: any): RequestContext;
    loginUser(username: string, password: string, options?: any): RequestContext;
    logoutUser(options?: any): RequestContext;
    updateUser(username: string, user: User, options?: any): RequestContext;
}
export declare class UserApiResponseProcessor {
    createUser(response: ResponseContext): void;
    createUsersWithArrayInput(response: ResponseContext): void;
    createUsersWithListInput(response: ResponseContext): void;
    deleteUser(response: ResponseContext): void;
    getUserByName(response: ResponseContext): User;
    loginUser(response: ResponseContext): string;
    logoutUser(response: ResponseContext): void;
    updateUser(response: ResponseContext): void;
}
