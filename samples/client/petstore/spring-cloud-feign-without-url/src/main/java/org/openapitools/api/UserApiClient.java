package org.openapitools.api;

import org.springframework.cloud.openfeign.FeignClient;
import org.openapitools.configuration.ClientConfiguration;

@FeignClient(name="${user.name:user}", configuration = ClientConfiguration.class)
public interface UserApiClient extends UserApi {
}
