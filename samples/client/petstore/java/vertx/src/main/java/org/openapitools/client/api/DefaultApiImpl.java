package org.openapitools.client.api;

import org.openapitools.client.model.XmlItem;

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


public class DefaultApiImpl implements DefaultApi {

    private ApiClient apiClient;

    public DefaultApiImpl() {
        this(null);
    }

    public DefaultApiImpl(ApiClient apiClient) {
        this.apiClient = apiClient != null ? apiClient : Configuration.getDefaultApiClient();
    }

    public ApiClient getApiClient() {
        return apiClient;
    }

    public void setApiClient(ApiClient apiClient) {
        this.apiClient = apiClient;
    }

    /**
     * creates an XmlItem
     * this route creates an XmlItem
     * @param xmlItem XmlItem Body (required)
     * @param resultHandler Asynchronous result handler
     */
    public void createXmlItem(XmlItem xmlItem, Handler<AsyncResult<Void>> resultHandler) {
        Object localVarBody = xmlItem;
        
        // verify the required parameter 'xmlItem' is set
        if (xmlItem == null) {
            resultHandler.handle(ApiException.fail(400, "Missing the required parameter 'xmlItem' when calling createXmlItem"));
            return;
        }
        
        // create path and map variables
        String localVarPath = "/fake/create_xml_item";

        // query params
        List<Pair> localVarQueryParams = new ArrayList<>();

        // header params
        MultiMap localVarHeaderParams = MultiMap.caseInsensitiveMultiMap();
        
        // form params
        // TODO: sending files within multipart/form-data is not supported yet (because of vertx web-client)
        Map<String, Object> localVarFormParams = new HashMap<>();
        
        String[] localVarAccepts = {  };
        String[] localVarContentTypes = { "application/xml", "application/xml; charset=utf-8", "application/xml; charset=utf-16", "text/xml", "text/xml; charset=utf-8", "text/xml; charset=utf-16" };
        String[] localVarAuthNames = new String[] {  };

        apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarBody, localVarHeaderParams, localVarFormParams, localVarAccepts, localVarContentTypes, localVarAuthNames, null, resultHandler);
    }
}
