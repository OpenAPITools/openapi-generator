package org.openapitools.client.api;

import org.openapitools.client.ApiClient;
import org.openapitools.client.EncodingUtils;
import org.openapitools.client.model.ApiResponse;

import org.openapitools.client.model.Pet;
import org.openapitools.client.model.TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import feign.*;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen")
public interface QueryApi extends ApiClient.Api {


  /**
   * Test query parameter(s)
   * Test query parameter(s)
   * @param integerQuery  (optional)
   * @param booleanQuery  (optional)
   * @param stringQuery  (optional)
   * @return String
   */
  @RequestLine("GET /query/integer/boolean/string?integer_query={integerQuery}&boolean_query={booleanQuery}&string_query={stringQuery}")
  @Headers({
    "Accept: text/plain",
  })
  String testQueryIntegerBooleanString(@Param("integerQuery") Integer integerQuery, @Param("booleanQuery") Boolean booleanQuery, @Param("stringQuery") String stringQuery);

  /**
   * Test query parameter(s)
   * Similar to <code>testQueryIntegerBooleanString</code> but it also returns the http response headers .
   * Test query parameter(s)
   * @param integerQuery  (optional)
   * @param booleanQuery  (optional)
   * @param stringQuery  (optional)
   * @return A ApiResponse that wraps the response boyd and the http headers.
   */
  @RequestLine("GET /query/integer/boolean/string?integer_query={integerQuery}&boolean_query={booleanQuery}&string_query={stringQuery}")
  @Headers({
    "Accept: text/plain",
  })
  ApiResponse<String> testQueryIntegerBooleanStringWithHttpInfo(@Param("integerQuery") Integer integerQuery, @Param("booleanQuery") Boolean booleanQuery, @Param("stringQuery") String stringQuery);


  /**
   * Test query parameter(s)
   * Test query parameter(s)
   * Note, this is equivalent to the other <code>testQueryIntegerBooleanString</code> method,
   * but with the query parameters collected into a single Map parameter. This
   * is convenient for services with optional query parameters, especially when
   * used with the {@link TestQueryIntegerBooleanStringQueryParams} class that allows for
   * building up this map in a fluent style.
   * @param queryParams Map of query parameters as name-value pairs
   *   <p>The following elements may be specified in the query map:</p>
   *   <ul>
   *   <li>integerQuery -  (optional)</li>
   *   <li>booleanQuery -  (optional)</li>
   *   <li>stringQuery -  (optional)</li>
   *   </ul>
   * @return String
   */
  @RequestLine("GET /query/integer/boolean/string?integer_query={integerQuery}&boolean_query={booleanQuery}&string_query={stringQuery}")
  @Headers({
  "Accept: text/plain",
  })
  String testQueryIntegerBooleanString(@QueryMap(encoded=true) Map<String, Object> queryParams);

  /**
  * Test query parameter(s)
  * Test query parameter(s)
  * Note, this is equivalent to the other <code>testQueryIntegerBooleanString</code> that receives the query parameters as a map,
  * but this one also exposes the Http response headers
      * @param queryParams Map of query parameters as name-value pairs
      *   <p>The following elements may be specified in the query map:</p>
      *   <ul>
          *   <li>integerQuery -  (optional)</li>
          *   <li>booleanQuery -  (optional)</li>
          *   <li>stringQuery -  (optional)</li>
      *   </ul>
          * @return String
      */
      @RequestLine("GET /query/integer/boolean/string?integer_query={integerQuery}&boolean_query={booleanQuery}&string_query={stringQuery}")
      @Headers({
    "Accept: text/plain",
      })
   ApiResponse<String> testQueryIntegerBooleanStringWithHttpInfo(@QueryMap(encoded=true) Map<String, Object> queryParams);


   /**
   * A convenience class for generating query parameters for the
   * <code>testQueryIntegerBooleanString</code> method in a fluent style.
   */
  public static class TestQueryIntegerBooleanStringQueryParams extends HashMap<String, Object> {
    public TestQueryIntegerBooleanStringQueryParams integerQuery(final Integer value) {
      put("integer_query", EncodingUtils.encode(value));
      return this;
    }
    public TestQueryIntegerBooleanStringQueryParams booleanQuery(final Boolean value) {
      put("boolean_query", EncodingUtils.encode(value));
      return this;
    }
    public TestQueryIntegerBooleanStringQueryParams stringQuery(final String value) {
      put("string_query", EncodingUtils.encode(value));
      return this;
    }
  }

  /**
   * Test query parameter(s)
   * Test query parameter(s)
   * @param queryObject  (optional)
   * @return String
   */
  @RequestLine("GET /query/style_form/explode_true/array_string?query_object={queryObject}")
  @Headers({
    "Accept: text/plain",
  })
  String testQueryStyleFormExplodeTrueArrayString(@Param("queryObject") TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter queryObject);

  /**
   * Test query parameter(s)
   * Similar to <code>testQueryStyleFormExplodeTrueArrayString</code> but it also returns the http response headers .
   * Test query parameter(s)
   * @param queryObject  (optional)
   * @return A ApiResponse that wraps the response boyd and the http headers.
   */
  @RequestLine("GET /query/style_form/explode_true/array_string?query_object={queryObject}")
  @Headers({
    "Accept: text/plain",
  })
  ApiResponse<String> testQueryStyleFormExplodeTrueArrayStringWithHttpInfo(@Param("queryObject") TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter queryObject);


  /**
   * Test query parameter(s)
   * Test query parameter(s)
   * Note, this is equivalent to the other <code>testQueryStyleFormExplodeTrueArrayString</code> method,
   * but with the query parameters collected into a single Map parameter. This
   * is convenient for services with optional query parameters, especially when
   * used with the {@link TestQueryStyleFormExplodeTrueArrayStringQueryParams} class that allows for
   * building up this map in a fluent style.
   * @param queryParams Map of query parameters as name-value pairs
   *   <p>The following elements may be specified in the query map:</p>
   *   <ul>
   *   <li>queryObject -  (optional)</li>
   *   </ul>
   * @return String
   */
  @RequestLine("GET /query/style_form/explode_true/array_string?query_object={queryObject}")
  @Headers({
  "Accept: text/plain",
  })
  String testQueryStyleFormExplodeTrueArrayString(@QueryMap(encoded=true) Map<String, Object> queryParams);

  /**
  * Test query parameter(s)
  * Test query parameter(s)
  * Note, this is equivalent to the other <code>testQueryStyleFormExplodeTrueArrayString</code> that receives the query parameters as a map,
  * but this one also exposes the Http response headers
      * @param queryParams Map of query parameters as name-value pairs
      *   <p>The following elements may be specified in the query map:</p>
      *   <ul>
          *   <li>queryObject -  (optional)</li>
      *   </ul>
          * @return String
      */
      @RequestLine("GET /query/style_form/explode_true/array_string?query_object={queryObject}")
      @Headers({
    "Accept: text/plain",
      })
   ApiResponse<String> testQueryStyleFormExplodeTrueArrayStringWithHttpInfo(@QueryMap(encoded=true) Map<String, Object> queryParams);


   /**
   * A convenience class for generating query parameters for the
   * <code>testQueryStyleFormExplodeTrueArrayString</code> method in a fluent style.
   */
  public static class TestQueryStyleFormExplodeTrueArrayStringQueryParams extends HashMap<String, Object> {
    public TestQueryStyleFormExplodeTrueArrayStringQueryParams queryObject(final TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter value) {
      put("query_object", EncodingUtils.encode(value));
      return this;
    }
  }

  /**
   * Test query parameter(s)
   * Test query parameter(s)
   * @param queryObject  (optional)
   * @return String
   */
  @RequestLine("GET /query/style_form/explode_true/object?query_object={queryObject}")
  @Headers({
    "Accept: text/plain",
  })
  String testQueryStyleFormExplodeTrueObject(@Param("queryObject") Pet queryObject);

  /**
   * Test query parameter(s)
   * Similar to <code>testQueryStyleFormExplodeTrueObject</code> but it also returns the http response headers .
   * Test query parameter(s)
   * @param queryObject  (optional)
   * @return A ApiResponse that wraps the response boyd and the http headers.
   */
  @RequestLine("GET /query/style_form/explode_true/object?query_object={queryObject}")
  @Headers({
    "Accept: text/plain",
  })
  ApiResponse<String> testQueryStyleFormExplodeTrueObjectWithHttpInfo(@Param("queryObject") Pet queryObject);


  /**
   * Test query parameter(s)
   * Test query parameter(s)
   * Note, this is equivalent to the other <code>testQueryStyleFormExplodeTrueObject</code> method,
   * but with the query parameters collected into a single Map parameter. This
   * is convenient for services with optional query parameters, especially when
   * used with the {@link TestQueryStyleFormExplodeTrueObjectQueryParams} class that allows for
   * building up this map in a fluent style.
   * @param queryParams Map of query parameters as name-value pairs
   *   <p>The following elements may be specified in the query map:</p>
   *   <ul>
   *   <li>queryObject -  (optional)</li>
   *   </ul>
   * @return String
   */
  @RequestLine("GET /query/style_form/explode_true/object?query_object={queryObject}")
  @Headers({
  "Accept: text/plain",
  })
  String testQueryStyleFormExplodeTrueObject(@QueryMap(encoded=true) Map<String, Object> queryParams);

  /**
  * Test query parameter(s)
  * Test query parameter(s)
  * Note, this is equivalent to the other <code>testQueryStyleFormExplodeTrueObject</code> that receives the query parameters as a map,
  * but this one also exposes the Http response headers
      * @param queryParams Map of query parameters as name-value pairs
      *   <p>The following elements may be specified in the query map:</p>
      *   <ul>
          *   <li>queryObject -  (optional)</li>
      *   </ul>
          * @return String
      */
      @RequestLine("GET /query/style_form/explode_true/object?query_object={queryObject}")
      @Headers({
    "Accept: text/plain",
      })
   ApiResponse<String> testQueryStyleFormExplodeTrueObjectWithHttpInfo(@QueryMap(encoded=true) Map<String, Object> queryParams);


   /**
   * A convenience class for generating query parameters for the
   * <code>testQueryStyleFormExplodeTrueObject</code> method in a fluent style.
   */
  public static class TestQueryStyleFormExplodeTrueObjectQueryParams extends HashMap<String, Object> {
    public TestQueryStyleFormExplodeTrueObjectQueryParams queryObject(final Pet value) {
      put("query_object", EncodingUtils.encode(value));
      return this;
    }
  }
}
