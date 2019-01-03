package org.openapitools.client.api;

import org.openapitools.client.CollectionFormats.*;

import io.reactivex.Observable;
import io.reactivex.Completable;
import retrofit2.http.*;

import okhttp3.RequestBody;
import okhttp3.ResponseBody;
import okhttp3.MultipartBody;

import org.openapitools.client.model.XmlItem;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public interface DefaultApi {
  /**
   * creates an XmlItem
   * this route creates an XmlItem
   * @param xmlItem XmlItem Body (required)
   * @return Completable
   */
  @Headers({
    "Content-Type:application/xml"
  })
  @POST("fake/create_xml_item")
  Completable createXmlItem(
    @retrofit2.http.Body XmlItem xmlItem
  );

}
