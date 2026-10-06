package org.openapitools.client.api;

import org.openapitools.client.CollectionFormats.*;

import io.reactivex.Observable;
import retrofit2.http.*;

import org.openapitools.client.model.Client;
import java.util.UUID;

public interface AnotherFakeApi {
  /**
   * To test special tags
   * To test special tags and operation ID starting with number
   * @param uuidTest to test uuid example value (required)
   * @param body client model (required)
   * @return Observable&lt;Client&gt;
   */
  @Headers({
    "Content-Type:application/json"
  })
  @PATCH("another-fake/dummy")
  Observable<Client> call123testSpecialTags(
    @retrofit2.http.Header("uuid_test") UUID uuidTest, @retrofit2.http.Body Client body
  );

}
