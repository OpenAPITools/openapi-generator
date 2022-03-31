package org.openapitools.client.model;

import java.util.Map;
import java.util.Collection;

public class HttpResponse<T>{

  private Map<String, Collection<String>> headers;

  private T body;

  private int status;

  public HttpResponse(Map<String, Collection<String>> headers, T body, int status) {
    this.headers = headers;
    this.body = body;
    this.status = status;
  }

  public T getBody(){
    return body;
  }

  public Map<String, Collection<String>> getHeaders(){
    return headers;
  }

  public int getStatus(){
    return status;
  }
}