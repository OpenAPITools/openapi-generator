import {
    Configuration,
    ConfigurationParameters,
} from "../runtime";

import {
    BehaviorApi,
} from "./BehaviorApi";
import {
    PetApi,
} from "./PetApi";
import {
    PetPartApi,
} from "./PetPartApi";
import {
    StoreApi,
} from "./StoreApi";
import {
    UserApi,
} from "./UserApi";

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
