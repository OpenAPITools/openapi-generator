import type { Configuration } from "../configuration";
import type { HttpFile, RequestContext, ResponseContext, HttpInfo } from "../http/http";

import { User } from "../models/User";

export abstract class AbstractUserApiRequestFactory {
    public abstract createUser(user: User, options?: Configuration): Promise<RequestContext>;

    public abstract createUsersWithArrayInput(user: Array<User>, options?: Configuration): Promise<RequestContext>;

    public abstract createUsersWithListInput(user: Array<User>, options?: Configuration): Promise<RequestContext>;

    public abstract deleteUser(username: string, options?: Configuration): Promise<RequestContext>;

    public abstract getUserByName(username: string, options?: Configuration): Promise<RequestContext>;

    public abstract loginUser(username: string, password: string, options?: Configuration): Promise<RequestContext>;

    public abstract logoutUser(options?: Configuration): Promise<RequestContext>;

    public abstract updateUser(username: string, user: User, options?: Configuration): Promise<RequestContext>;

}


export abstract class AbstractUserApiResponseProcessor {
     public abstract createUserWithHttpInfo(response: ResponseContext): Promise<HttpInfo< void>>;

     public abstract createUsersWithArrayInputWithHttpInfo(response: ResponseContext): Promise<HttpInfo< void>>;

     public abstract createUsersWithListInputWithHttpInfo(response: ResponseContext): Promise<HttpInfo< void>>;

     public abstract deleteUserWithHttpInfo(response: ResponseContext): Promise<HttpInfo< void>>;

     public abstract getUserByNameWithHttpInfo(response: ResponseContext): Promise<HttpInfo<User >>;

     public abstract loginUserWithHttpInfo(response: ResponseContext): Promise<HttpInfo<string >>;

     public abstract logoutUserWithHttpInfo(response: ResponseContext): Promise<HttpInfo< void>>;

     public abstract updateUserWithHttpInfo(response: ResponseContext): Promise<HttpInfo< void>>;

}
