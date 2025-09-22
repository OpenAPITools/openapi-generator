package org.openapitools.api;

import org.springframework.cloud.openfeign.FeignClient;
import org.openapitools.configuration.ClientConfiguration;

@FeignClient(name="${some.name:some}", contextId="${some.contextId:${some.name}}", url="${some.url:http://localhost}", configuration = ClientConfiguration.class)
public interface SomeApiClient extends SomeApi {
}
