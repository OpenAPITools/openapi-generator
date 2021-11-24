package org.openapitools.client;

import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;

import java.util.Objects;

public class Configuration {

    private static ApiClient defaultApiClient = null;

    /**
     * Setup the default API client.
     * Will be used by API instances when a client is not provided.
     *
     * @return Default API client
     */
    public synchronized static ApiClient setupDefaultApiClient(Vertx vertx, JsonObject config) {
        defaultApiClient = new ApiClient(vertx, config);
        return defaultApiClient;
    }

    /**
     * Get the default API client, which would be used when creating API
     * instances without providing an API client.
     *
     * @return Default API client
     */
    public synchronized static ApiClient getDefaultApiClient() {
        return defaultApiClient;
    }

    /**
     * Set the default API client, which would be used when creating API
     * instances without providing an API client.
     *
     * @param apiClient API client
     */
    public synchronized static void setDefaultApiClient(ApiClient apiClient) {
        defaultApiClient = apiClient;
    }
}
