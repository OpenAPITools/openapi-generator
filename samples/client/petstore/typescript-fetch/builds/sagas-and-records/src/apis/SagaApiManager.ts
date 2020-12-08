import {
	Configuration,
	ConfigurationParameters,
} from "../";

import {
    AffiliateApi,
    BrandApi,
    PetApi,
    StoreApi,
    UserApi,
} from "./";

export class Api {
	public static affiliateApi: AffiliateApi;
	public static brandApi: BrandApi;
	public static petApi: PetApi;
	public static storeApi: StoreApi;
	public static userApi: UserApi;

	public static init(apiBasePath: string) {
		const apiBaseConfig: ConfigurationParameters = {
			basePath: apiBasePath,
			credentials: "include",
			headers: {
				'Cache-Control': 'no-cache, no-store' // this is needed to prevent stalling issues in Chrome. Also it is a good behavior for api calls.
			}
		};

		Api.affiliateApi = new AffiliateApi(new Configuration(apiBaseConfig));
		Api.brandApi = new BrandApi(new Configuration(apiBaseConfig));
		Api.petApi = new PetApi(new Configuration(apiBaseConfig));
		Api.storeApi = new StoreApi(new Configuration(apiBaseConfig));
		Api.userApi = new UserApi(new Configuration(apiBaseConfig));
	}
}