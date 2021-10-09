package org.openapitools.client.model;

import java.util.Map;
import java.util.Collection;

public class HttpResponse<T>{

  private Map<String, Collection<String>> headers;

  private T body;

  public HttpResponse(Map<String, Collection<String>> headers, T body) {
    this.headers = headers;
    this.body = body;
  }

  public T getBody(){
    return body;
  }

  public Map<String, Collection<String>> getHeaders(){
    return headers;
  }
}