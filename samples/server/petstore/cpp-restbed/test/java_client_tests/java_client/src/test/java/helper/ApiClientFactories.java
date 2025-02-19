package helper;

import org.openapi.example.api.PetApi;
import org.openapi.example.api.StoreApi;
import org.openapi.example.api.UserApi;
import org.openapi.example.invoker.ApiClient;

public class ApiClientFactories {
    public static PetApi setUpPetApi(int port) {
        return new PetApi(setUpDefaultApiClient(port));
    }

    public static StoreApi setUpStoreApi(int port) {
        return new StoreApi(setUpDefaultApiClient(port));
    }
    public static UserApi setUpUserApi(int port) {
        return new UserApi(setUpDefaultApiClient(port));
    }

    private static ApiClient setUpDefaultApiClient(int port) {
        ApiClient defaultClient = new ApiClient();
        defaultClient.setBasePath("http://localhost:" + String.valueOf(port) + "/v2");
        return defaultClient;
    }
}
