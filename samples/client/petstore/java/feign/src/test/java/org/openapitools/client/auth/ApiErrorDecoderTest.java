package org.openapitools.client.auth;

import feign.FeignException;
import feign.Request;
import feign.Response;
import feign.RetryableException;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import java.util.Collections;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

class ApiErrorDecoderTest {

    private final ApiErrorDecoder apiErrorDecoder = new ApiErrorDecoder();

    @Test
    void decode400() {
        Response response = getDummyResponse(400);

        FeignException.BadRequest badRequest = (FeignException.BadRequest) apiErrorDecoder.decode("GET", response);
        assertThat(badRequest.status(), is(400));
    }

    @ParameterizedTest
    @ValueSource(ints = {401, 403})
    void decodeAuthorizationErrors(Integer httpStatus) {
        Response response = getDummyResponse(httpStatus);

        RetryableException retryableException = (RetryableException) apiErrorDecoder.decode("GET", response);
        assertThat(retryableException.status(), is(httpStatus));
        assertThat(retryableException.retryAfter(), is(nullValue()));
    }

    private Response getDummyResponse(Integer httpStatus) {
        Request request = Request.create(Request.HttpMethod.GET, "http://localhost", Collections.emptyMap(), Request.Body.empty(), null);
        return Response.builder()
                .status(httpStatus)
                .request(request)
                .build();
    }
}