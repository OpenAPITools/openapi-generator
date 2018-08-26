package org.openapitools.client.api;

import org.openapitools.client.CollectionFormats.*;
import org.openapitools.client.model.Client;
import retrofit2.Response;
import retrofit2.http.*;

public interface AnotherFakeApi {
  /**
   * To test special tags
   * To test special tags and operation ID starting with number
   * @param client client model (required)
   * @return Call&lt;Client&gt;
   */
  @Headers({
    "Content-Type:application/json"
  })
  @PATCH("another-fake/dummy")
  CompletionStage<Response<Client>> call123testSpecialTags(
    @retrofit2.http.Body Client client
  );

}
