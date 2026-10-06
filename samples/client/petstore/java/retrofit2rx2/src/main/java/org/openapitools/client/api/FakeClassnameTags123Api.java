package org.openapitools.client.api;

import org.openapitools.client.CollectionFormats.*;

import io.reactivex.Observable;
import retrofit2.http.*;

import org.openapitools.client.model.Client;

public interface FakeClassnameTags123Api {
  /**
   * To test class name in snake case
   * To test class name in snake case
   * @param body client model (required)
   * @return Observable&lt;Client&gt;
   */
  @Headers({
    "Content-Type:application/json"
  })
  @PATCH("fake_classname_test")
  Observable<Client> testClassname(
    @retrofit2.http.Body Client body
  );

}
