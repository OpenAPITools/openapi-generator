package io.swagger.api;

import org.springframework.cloud.netflix.feign.FeignClient;
import io.swagger.configuration.ClientConfiguration;

@FeignClient(name="${swaggerPetstore.name:swaggerPetstore}", url="${swaggerPetstore.url:http://petstore.swagger.io/v2}", configuration = ClientConfiguration.class)
public interface PetApiClient extends PetApi {
}