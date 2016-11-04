package io.swagger.client.api;

import io.swagger.client.ApiClient;

import io.swagger.client.model.Client;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import feign.*;


public interface FakeclassnametagsApi extends ApiClient.Api {


  /**
   * To test class name in snake case
   * 
   * @param body client model (required)
   * @return Client
   */
  @RequestLine("PATCH /fake_classname_test")
  @Headers({
    "Content-type: application/json",
    "Accept: application/json",
  })
  Client testClassname(Client body);
}
