package org.openapitools.client.api;

import org.openapitools.client.CollectionFormats.*;

import retrofit.Callback;
import retrofit.http.*;
import retrofit.mime.*;

import org.openapitools.client.model.XmlItem;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public interface DefaultApi {
  /**
   * creates an XmlItem
   * Sync method
   * this route creates an XmlItem
   * @param xmlItem XmlItem Body (required)
   * @return Void
   */
  
  @POST("/fake/create_xml_item")
  Void createXmlItem(
    @retrofit.http.Body XmlItem xmlItem
  );

  /**
   * creates an XmlItem
   * Async method
   * @param xmlItem XmlItem Body (required)
   * @param cb callback method
   */
  
  @POST("/fake/create_xml_item")
  void createXmlItem(
    @retrofit.http.Body XmlItem xmlItem, Callback<Void> cb
  );
}
