package org.openapitools.client.api;

import org.openapitools.client.ApiClient;
import org.openapitools.client.EncodingUtils;

import org.openapitools.client.model.Client;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import feign.*;


public interface AnotherFakeApi extends ApiClient.Api {


  /**
   * To test special tags
   * To test special tags and operation ID starting with number
   * @param body client model (required)
   * @return Client
   */
  @RequestLine("PATCH /another-fake/dummy")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
  })
  Client call123testSpecialTags(Client body);
}
