package org.openapitools.api;

import org.springframework.cloud.openfeign.FeignClient;
import org.openapitools.configuration.ClientConfiguration;

@FeignClient(name="${openAPIPetstore.name:openAPIPetstore}", url="${openAPIPetstore.url:https://petstore.swagger.io/v2}", configuration = ClientConfiguration.class)
public interface PetApiClient extends PetApi {
}