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
public interface FakeClassnameTags123Api extends ApiClient.Api {


  /**
   * To test class name in snake case
   * To test class name in snake case
   * @param client client model (required)
   * @return Client
   */
  @RequestLine("PATCH /fake_classname_test")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
  })
  Client testClassname(Client client);

  /**
   * To test class name in snake case
   * Similar to <code>testClassname</code> but it also returns the http response headers .
   * To test class name in snake case
   * @param client client model (required)
   * @return A ApiResponse that wraps the response boyd and the http headers.
   */
  @RequestLine("PATCH /fake_classname_test")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
  })
  ApiResponse<Client> testClassnameWithHttpInfo(Client client);


}
