package io.swagger.client.api;

import io.swagger.client.ApiClient;
import io.swagger.client.EncodingUtils;

import io.swagger.client.model.Client;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import feign.*;


public interface AnotherFakeApi extends ApiClient.Api {


  /**
   * To test special tags
   * To test special tags
    * @param body client model (required)
   * @return Client
   */
  @RequestLine("PATCH /another-fake/dummy")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
  })
  Client testSpecialTags(Client body);
}
