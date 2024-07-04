package org.openapitools.api;

import org.springframework.cloud.openfeign.FeignClient;
import org.openapitools.configuration.ClientConfiguration;

@FeignClient(name="${pet.name:pet}", configuration = ClientConfiguration.class)
public interface PetApiClient extends PetApi {
}
