package org.openapitools.client.api;

import org.openapitools.client.CollectionFormats.*;

import retrofit.Callback;
import retrofit.http.*;
import retrofit.mime.*;

import org.openapitools.client.model.Client;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public interface AnotherFakeApi {
  /**
   * To test special tags
   * Sync method
   * To test special tags and operation ID starting with number
   * @param body client model (required)
   * @return Client
   */
  
  @PATCH("/another-fake/dummy")
  Client call123testSpecialTags(
    @retrofit.http.Body Client body
  );

  /**
   * To test special tags
   * Async method
   * @param body client model (required)
   * @param cb callback method
   */
  
  @PATCH("/another-fake/dummy")
  void call123testSpecialTags(
    @retrofit.http.Body Client body, Callback<Client> cb
  );
}
