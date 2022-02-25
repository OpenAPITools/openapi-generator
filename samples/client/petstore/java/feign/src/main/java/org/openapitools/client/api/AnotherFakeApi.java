package org.openapitools.client.api;

import org.openapitools.client.ApiClient;
import org.openapitools.client.EncodingUtils;
import org.openapitools.client.model.ApiResponse;

import org.openapitools.client.model.Client;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import feign.*;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen")
public interface AnotherFakeApi extends ApiClient.Api {


  /**
   * To test special tags
   * To test special tags and operation ID starting with number
   * @param client client model (required)
   * @return Client
   */
  @RequestLine("PATCH /another-fake/dummy")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
  })
  Client call123testSpecialTags(Client client);

  /**
   * To test special tags
   * Similar to <code>call123testSpecialTags</code> but it also returns the http response headers .
   * To test special tags and operation ID starting with number
   * @param client client model (required)
   * @return A ApiResponse that wraps the response boyd and the http headers.
   */
  @RequestLine("PATCH /another-fake/dummy")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
  })
  ApiResponse<Client> call123testSpecialTagsWithHttpInfo(Client client);


}
