package io.swagger.client.api;

import io.swagger.client.CollectionFormats.*;

import retrofit2.Call;
import retrofit2.http.*;

import okhttp3.RequestBody;
import okhttp3.ResponseBody;
import okhttp3.MultipartBody;

import io.swagger.client.model.Client;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

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
  Call<Client> testClassname(
    @retrofit2.http.Body Client body
  );

}
