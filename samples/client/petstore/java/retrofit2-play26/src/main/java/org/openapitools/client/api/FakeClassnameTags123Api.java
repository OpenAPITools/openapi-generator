package org.openapitools.client.api;

import org.openapitools.client.CollectionFormats.*;

import retrofit2.http.*;

import org.openapitools.client.model.Client;

import jakarta.validation.constraints.*;

import java.util.concurrent.*;
import retrofit2.Response;

public interface FakeClassnameTags123Api {
  /**
   * To test class name in snake case
   * To test class name in snake case
   * @param body client model (required)
   * @return Call&lt;Client&gt;
   */
  @Headers({
    "Content-Type:application/json"
  })
  @PATCH("fake_classname_test")
  CompletionStage<Response<Client>> testClassname(
    @retrofit2.http.Body Client body
  );

}
