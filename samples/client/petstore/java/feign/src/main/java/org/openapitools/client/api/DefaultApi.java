package org.openapitools.client.api;

import org.openapitools.client.ApiClient;
import org.openapitools.client.EncodingUtils;

import org.openapitools.client.model.XmlItem;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import feign.*;


public interface DefaultApi extends ApiClient.Api {


  /**
   * creates an XmlItem
   * this route creates an XmlItem
   * @param xmlItem XmlItem Body (required)
   */
  @RequestLine("POST /fake/create_xml_item")
  @Headers({
    "Content-Type: application/xml",
    "Accept: application/json",
  })
  void createXmlItem(XmlItem xmlItem);
}
