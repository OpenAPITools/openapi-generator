package org.openapitools.client.api;

import org.openapitools.client.model.Client;

import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.MultiMap;
import io.vertx.core.json.JsonObject;

import com.fasterxml.jackson.core.type.TypeReference;

import java.util.*;

import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.Pair;


public class AnotherFakeApiImpl implements AnotherFakeApi {

    private ApiClient apiClient;

    public AnotherFakeApiImpl() {
        this(null);
    }

    public AnotherFakeApiImpl(ApiClient apiClient) {
        this.apiClient = apiClient != null ? apiClient : Configuration.getDefaultApiClient();
    }

    public ApiClient getApiClient() {
        return apiClient;
    }

    public void setApiClient(ApiClient apiClient) {
        this.apiClient = apiClient;
    }

    /**
     * To test special tags
     * To test special tags and operation ID starting with number
     * @param client client model (required)
     * @param resultHandler Asynchronous result handler
     */
    public void call123testSpecialTags(Client client, Handler<AsyncResult<Client>> resultHandler) {
        Object localVarBody = client;
        
        // verify the required parameter 'client' is set
        if (client == null) {
            resultHandler.handle(ApiException.fail(400, "Missing the required parameter 'client' when calling call123testSpecialTags"));
            return;
        }
        
        // create path and map variables
        String localVarPath = "/another-fake/dummy";

        // query params
        List<Pair> localVarQueryParams = new ArrayList<>();

        // header params
        MultiMap localVarHeaderParams = MultiMap.caseInsensitiveMultiMap();
        
        // form params
        // TODO: sending files within multipart/form-data is not supported yet (because of vertx web-client)
        Map<String, Object> localVarFormParams = new HashMap<>();
        
        String[] localVarAccepts = { "application/json" };
        String[] localVarContentTypes = { "application/json" };
        String[] localVarAuthNames = new String[] {  };
        TypeReference<Client> localVarReturnType = new TypeReference<Client>() {};
        apiClient.invokeAPI(localVarPath, "PATCH", localVarQueryParams, localVarBody, localVarHeaderParams, localVarFormParams, localVarAccepts, localVarContentTypes, localVarAuthNames, localVarReturnType, resultHandler);
    }
}
