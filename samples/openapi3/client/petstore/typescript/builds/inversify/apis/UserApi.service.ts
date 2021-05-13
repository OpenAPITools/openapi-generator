import type { Configuration } from "../configuration";
import type { HttpFile, RequestContext, ResponseContext } from "../http/http";

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
     public abstract createUser(response: ResponseContext): Promise< void>;

     public abstract createUsersWithArrayInput(response: ResponseContext): Promise< void>;

     public abstract createUsersWithListInput(response: ResponseContext): Promise< void>;

     public abstract deleteUser(response: ResponseContext): Promise< void>;

     public abstract getUserByName(response: ResponseContext): Promise<User >;

     public abstract loginUser(response: ResponseContext): Promise<string >;

     public abstract logoutUser(response: ResponseContext): Promise< void>;

     public abstract updateUser(response: ResponseContext): Promise< void>;

}
