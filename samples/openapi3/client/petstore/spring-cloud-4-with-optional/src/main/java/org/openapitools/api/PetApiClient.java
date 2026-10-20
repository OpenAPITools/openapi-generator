package org.openapitools.api;

import org.springframework.cloud.openfeign.FeignClient;
import org.openapitools.configuration.ClientConfiguration;

@FeignClient(name="${pet.name:pet}", contextId="${pet.contextId:${pet.name}}", url="${pet.url:http://petstore.swagger.io/v2}", configuration = ClientConfiguration.class)
public interface PetApiClient extends PetApi {
}
