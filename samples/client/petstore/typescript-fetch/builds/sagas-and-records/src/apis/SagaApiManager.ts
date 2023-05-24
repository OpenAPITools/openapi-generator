import {
    Configuration,
    ConfigurationParameters,
} from "../";

import {
    BehaviorApi,
    PetApi,
    PetPartApi,
    StoreApi,
    UserApi,
} from "./";

export class Api {
    public static behaviorApi: BehaviorApi;
    public static petApi: PetApi;
    public static petPartApi: PetPartApi;
    public static storeApi: StoreApi;
    public static userApi: UserApi;

    public static init(apiBaseConfig: ConfigurationParameters) {
        Api.behaviorApi = new BehaviorApi(new Configuration(apiBaseConfig));
        Api.petApi = new PetApi(new Configuration(apiBaseConfig));
        Api.petPartApi = new PetPartApi(new Configuration(apiBaseConfig));
        Api.storeApi = new StoreApi(new Configuration(apiBaseConfig));
        Api.userApi = new UserApi(new Configuration(apiBaseConfig));
    }
}