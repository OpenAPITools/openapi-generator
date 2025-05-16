package org.openapitools.client.api;

import org.openapitools.client.ApiException;
import org.openapitools.client.ApiClient;
import org.openapitools.client.Configuration;
import org.openapitools.client.Pair;

import javax.ws.rs.core.GenericType;

import org.openapitools.client.model.DataQuery;
import java.time.LocalDate;
import java.time.OffsetDateTime;
import org.openapitools.client.model.Pet;
import org.openapitools.client.model.StringEnumRef;
import org.openapitools.client.model.TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter;
import org.openapitools.client.model.TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen", comments = "Generator version: 7.14.0-SNAPSHOT")
public class QueryApi {
  private ApiClient apiClient;

  public QueryApi() {
    this(Configuration.getDefaultApiClient());
  }

  public QueryApi(ApiClient apiClient) {
    this.apiClient = apiClient;
  }

  public ApiClient getApiClient() {
    return apiClient;
  }

  public void setApiClient(ApiClient apiClient) {
    this.apiClient = apiClient;
  }

  /**
   * Test query parameter(s)
   * Test query parameter(s)
   * @param enumNonrefStringQuery  (optional)
   * @param enumRefStringQuery  (optional)
   * @return a {@code String}
   * @throws ApiException if fails to make API call
   */
  public String testEnumRefString(@javax.annotation.Nullable String enumNonrefStringQuery, @javax.annotation.Nullable StringEnumRef enumRefStringQuery) throws ApiException {
    Object localVarPostBody = null;
    
    // create path and map variables
    String localVarPath = "/query/enum_ref_string".replaceAll("\\{format\\}","json");

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, String> localVarCookieParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();

    localVarQueryParams.addAll(apiClient.parameterToPairs("", "enum_nonref_string_query", enumNonrefStringQuery));
    localVarQueryParams.addAll(apiClient.parameterToPairs("", "enum_ref_string_query", enumRefStringQuery));

    
    
    
    final String[] localVarAccepts = {
      "text/plain"
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {  };

    GenericType<String> localVarReturnType = new GenericType<String>() {};
    return apiClient.invokeAPI(localVarPath, "GET", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
      }
  /**
   * Test query parameter(s)
   * Test query parameter(s)
   * @param datetimeQuery  (optional)
   * @param dateQuery  (optional)
   * @param stringQuery  (optional)
   * @return a {@code String}
   * @throws ApiException if fails to make API call
   */
  public String testQueryDatetimeDateString(@javax.annotation.Nullable OffsetDateTime datetimeQuery, @javax.annotation.Nullable LocalDate dateQuery, @javax.annotation.Nullable String stringQuery) throws ApiException {
    Object localVarPostBody = null;
    
    // create path and map variables
    String localVarPath = "/query/datetime/date/string".replaceAll("\\{format\\}","json");

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, String> localVarCookieParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();

    localVarQueryParams.addAll(apiClient.parameterToPairs("", "datetime_query", datetimeQuery));
    localVarQueryParams.addAll(apiClient.parameterToPairs("", "date_query", dateQuery));
    localVarQueryParams.addAll(apiClient.parameterToPairs("", "string_query", stringQuery));

    
    
    
    final String[] localVarAccepts = {
      "text/plain"
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {  };

    GenericType<String> localVarReturnType = new GenericType<String>() {};
    return apiClient.invokeAPI(localVarPath, "GET", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
      }
  /**
   * Test query parameter(s)
   * Test query parameter(s)
   * @param integerQuery  (optional)
   * @param booleanQuery  (optional)
   * @param stringQuery  (optional)
   * @return a {@code String}
   * @throws ApiException if fails to make API call
   */
  public String testQueryIntegerBooleanString(@javax.annotation.Nullable Integer integerQuery, @javax.annotation.Nullable Boolean booleanQuery, @javax.annotation.Nullable String stringQuery) throws ApiException {
    Object localVarPostBody = null;
    
    // create path and map variables
    String localVarPath = "/query/integer/boolean/string".replaceAll("\\{format\\}","json");

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, String> localVarCookieParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();

    localVarQueryParams.addAll(apiClient.parameterToPairs("", "integer_query", integerQuery));
    localVarQueryParams.addAll(apiClient.parameterToPairs("", "boolean_query", booleanQuery));
    localVarQueryParams.addAll(apiClient.parameterToPairs("", "string_query", stringQuery));

    
    
    
    final String[] localVarAccepts = {
      "text/plain"
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {  };

    GenericType<String> localVarReturnType = new GenericType<String>() {};
    return apiClient.invokeAPI(localVarPath, "GET", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
      }
  /**
   * Test query parameter(s)
   * Test query parameter(s)
   * @param queryObject  (optional)
   * @return a {@code String}
   * @throws ApiException if fails to make API call
   */
  public String testQueryStyleDeepObjectExplodeTrueObject(@javax.annotation.Nullable Pet queryObject) throws ApiException {
    Object localVarPostBody = null;
    
    // create path and map variables
    String localVarPath = "/query/style_deepObject/explode_true/object".replaceAll("\\{format\\}","json");

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, String> localVarCookieParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();

    localVarQueryParams.addAll(apiClient.parameterToPairs("", "query_object", queryObject));

    
    
    
    final String[] localVarAccepts = {
      "text/plain"
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {  };

    GenericType<String> localVarReturnType = new GenericType<String>() {};
    return apiClient.invokeAPI(localVarPath, "GET", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
      }
  /**
   * Test query parameter(s)
   * Test query parameter(s)
   * @param queryObject  (optional)
   * @return a {@code String}
   * @throws ApiException if fails to make API call
   */
  public String testQueryStyleDeepObjectExplodeTrueObjectAllOf(@javax.annotation.Nullable TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter queryObject) throws ApiException {
    Object localVarPostBody = null;
    
    // create path and map variables
    String localVarPath = "/query/style_deepObject/explode_true/object/allOf".replaceAll("\\{format\\}","json");

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, String> localVarCookieParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();

    localVarQueryParams.addAll(apiClient.parameterToPairs("", "query_object", queryObject));

    
    
    
    final String[] localVarAccepts = {
      "text/plain"
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {  };

    GenericType<String> localVarReturnType = new GenericType<String>() {};
    return apiClient.invokeAPI(localVarPath, "GET", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
      }
  /**
   * Test query parameter(s)
   * Test query parameter(s)
   * @param queryObject  (optional)
   * @return a {@code String}
   * @throws ApiException if fails to make API call
   */
  public String testQueryStyleFormExplodeFalseArrayInteger(@javax.annotation.Nullable List<Integer> queryObject) throws ApiException {
    Object localVarPostBody = null;
    
    // create path and map variables
    String localVarPath = "/query/style_form/explode_false/array_integer".replaceAll("\\{format\\}","json");

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, String> localVarCookieParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();

    localVarQueryParams.addAll(apiClient.parameterToPairs("csv", "query_object", queryObject));

    
    
    
    final String[] localVarAccepts = {
      "text/plain"
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {  };

    GenericType<String> localVarReturnType = new GenericType<String>() {};
    return apiClient.invokeAPI(localVarPath, "GET", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
      }
  /**
   * Test query parameter(s)
   * Test query parameter(s)
   * @param queryObject  (optional)
   * @return a {@code String}
   * @throws ApiException if fails to make API call
   */
  public String testQueryStyleFormExplodeFalseArrayString(@javax.annotation.Nullable List<String> queryObject) throws ApiException {
    Object localVarPostBody = null;
    
    // create path and map variables
    String localVarPath = "/query/style_form/explode_false/array_string".replaceAll("\\{format\\}","json");

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, String> localVarCookieParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();

    localVarQueryParams.addAll(apiClient.parameterToPairs("csv", "query_object", queryObject));

    
    
    
    final String[] localVarAccepts = {
      "text/plain"
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {  };

    GenericType<String> localVarReturnType = new GenericType<String>() {};
    return apiClient.invokeAPI(localVarPath, "GET", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
      }
  /**
   * Test query parameter(s)
   * Test query parameter(s)
   * @param queryObject  (optional)
   * @return a {@code String}
   * @throws ApiException if fails to make API call
   */
  public String testQueryStyleFormExplodeTrueArrayString(@javax.annotation.Nullable TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter queryObject) throws ApiException {
    Object localVarPostBody = null;
    
    // create path and map variables
    String localVarPath = "/query/style_form/explode_true/array_string".replaceAll("\\{format\\}","json");

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, String> localVarCookieParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();

    localVarQueryParams.addAll(apiClient.parameterToPairs("", "query_object", queryObject));

    
    
    
    final String[] localVarAccepts = {
      "text/plain"
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {  };

    GenericType<String> localVarReturnType = new GenericType<String>() {};
    return apiClient.invokeAPI(localVarPath, "GET", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
      }
  /**
   * Test query parameter(s)
   * Test query parameter(s)
   * @param queryObject  (optional)
   * @return a {@code String}
   * @throws ApiException if fails to make API call
   */
  public String testQueryStyleFormExplodeTrueObject(@javax.annotation.Nullable Pet queryObject) throws ApiException {
    Object localVarPostBody = null;
    
    // create path and map variables
    String localVarPath = "/query/style_form/explode_true/object".replaceAll("\\{format\\}","json");

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, String> localVarCookieParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();

    localVarQueryParams.addAll(apiClient.parameterToPairs("", "query_object", queryObject));

    
    
    
    final String[] localVarAccepts = {
      "text/plain"
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {  };

    GenericType<String> localVarReturnType = new GenericType<String>() {};
    return apiClient.invokeAPI(localVarPath, "GET", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
      }
  /**
   * Test query parameter(s)
   * Test query parameter(s)
   * @param queryObject  (optional)
   * @return a {@code String}
   * @throws ApiException if fails to make API call
   */
  public String testQueryStyleFormExplodeTrueObjectAllOf(@javax.annotation.Nullable DataQuery queryObject) throws ApiException {
    Object localVarPostBody = null;
    
    // create path and map variables
    String localVarPath = "/query/style_form/explode_true/object/allOf".replaceAll("\\{format\\}","json");

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, String> localVarCookieParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();

    localVarQueryParams.addAll(apiClient.parameterToPairs("", "query_object", queryObject));

    
    
    
    final String[] localVarAccepts = {
      "text/plain"
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {  };

    GenericType<String> localVarReturnType = new GenericType<String>() {};
    return apiClient.invokeAPI(localVarPath, "GET", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
      }
}
