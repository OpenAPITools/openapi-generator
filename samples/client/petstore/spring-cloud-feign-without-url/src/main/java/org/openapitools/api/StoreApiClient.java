package org.openapitools.api;

import org.springframework.cloud.openfeign.FeignClient;
import org.openapitools.configuration.ClientConfiguration;

@FeignClient(name="${store.name:store}", contextId="${store.contextId:${store.name}}",  configuration = ClientConfiguration.class)
public interface StoreApiClient extends StoreApi {
}
