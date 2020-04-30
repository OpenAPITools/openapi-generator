package org.openapitools.client.api;

import org.openapitools.client.CollectionFormats.*;

import io.reactivex.Observable;
import io.reactivex.Completable;
import retrofit2.http.*;

import okhttp3.RequestBody;
import okhttp3.ResponseBody;
import okhttp3.MultipartBody;

import org.openapitools.client.model.Client;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public interface AnotherFakeApi {
  /**
   * To test special tags
   * To test special tags and operation ID starting with number
   * @param body client model (required)
   * @return Observable&lt;Client&gt;
   */
  @Headers({
    "Content-Type:application/json"
  })
  @PATCH("another-fake/dummy")
  Observable<Client> call123testSpecialTags(
    @retrofit2.http.Body Client body
  );

}
