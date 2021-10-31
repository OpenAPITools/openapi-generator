package org.openapitools.client.auth;

import feign.Response;
import feign.RetryableException;
import feign.codec.ErrorDecoder;

/**
 * Error decoder that makes the HTTP 401 and 403 Retryable. Sometimes the 401 or 403 may indicate an expired token
 * All the other HTTP status are handled by the {@link feign.codec.ErrorDecoder.Default} decoder
 */
public class ApiErrorDecoder implements ErrorDecoder {

    private final Default defaultErrorDecoder = new Default();

    @Override
    public Exception decode(String methodKey, Response response) {
        //401/403 response codes most likely indicate an expired access token, unless it happens two times in a row
        Exception httpException = defaultErrorDecoder.decode(methodKey, response);
        if (response.status() == 401 || response.status() == 403) {
            return new RetryableException(response.status(), "Received status " + response.status() + " trying to renew access token",
                    response.request().httpMethod(), httpException, null, response.request());
        }
        return httpException;
    }
}