package io.swagger.client.api;

import io.swagger.client.CollectionFormats.*;

import retrofit.Callback;
import retrofit.http.*;
import retrofit.mime.*;

import io.swagger.client.model.Client;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public interface FakeClassnameTags123Api {
  /**
   * To test class name in snake case
   * Sync method
   * To test class name in snake case
   * @param body client model (required)
   * @return Client
   */
  
  @PATCH("/fake_classname_test")
  Client testClassname(
    @retrofit.http.Body Client body
  );

  /**
   * To test class name in snake case
   * Async method
   * @param body client model (required)
   * @param cb callback method
   */
  
  @PATCH("/fake_classname_test")
  void testClassname(
    @retrofit.http.Body Client body, Callback<Client> cb
  );
}
