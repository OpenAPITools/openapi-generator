package org.openapitools.client.api;

import org.openapitools.client.ApiClient;
import org.openapitools.client.EncodingUtils;

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
   * @param body client model (required)
   * @return Client
   */
  @RequestLine("PATCH /fake_classname_test")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
  })
  Client testClassname(Client body);
}
