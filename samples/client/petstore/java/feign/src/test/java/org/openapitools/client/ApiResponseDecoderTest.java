package org.openapitools.client;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.collect.ImmutableMap;
import feign.Request;
import feign.Request.HttpMethod;
import feign.RequestTemplate;
import feign.Response;
import java.io.IOException;
import java.util.Collection;
import java.util.Collections;
import org.junit.jupiter.api.Test;
import org.openapitools.client.model.ApiResponse;
import org.openapitools.client.model.Cat;

class ApiResponseDecoderTest {

  private final ObjectMapper objectMapper = new ObjectMapper();

  ApiResponseDecoder decoder = new ApiResponseDecoder(objectMapper);


  @Test
  void shouldDecodeApiResponseWithHeaders() throws IOException {

    final Cat cat = makeCat();
    TypeReference<ApiResponse<Cat>> typeReference = new TypeReference<ApiResponse<Cat>>() {
    };

    ApiResponse<Cat> response = (ApiResponse<Cat>) decoder.decode(Response.builder()
        .headers(ImmutableMap.of("Location",
            Collections.singletonList("https://example.com/cats/1")))
        .body(objectMapper.writeValueAsBytes(cat))
        .status(201)
        .request(buildRequest())
        .build(), typeReference.getType());

    assertEquals(cat.getColor(), response.getData().getColor());
    assertEquals(cat.isDeclawed(), response.getData().isDeclawed());

    Collection<String> locationValues = response.getHeaders().get("Location");
    assertEquals(1, locationValues.size());
    assertEquals("https://example.com/cats/1", locationValues.iterator().next());
  }

  private Cat makeCat() {
    final Cat cat = new Cat();
    cat.color("black");
    cat.declawed(true);
    return cat;
  }

  private Request buildRequest() {
    return Request.create(HttpMethod.GET, "https://example.com/cats/1", Collections.emptyMap(),
        null, new RequestTemplate());
  }
}