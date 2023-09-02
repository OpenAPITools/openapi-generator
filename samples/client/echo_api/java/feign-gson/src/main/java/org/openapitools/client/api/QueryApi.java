package org.openapitools.client.api;

import org.openapitools.client.ApiClient;
import org.openapitools.client.EncodingUtils;
import org.openapitools.client.model.ApiResponse;

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
import feign.*;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen")
public interface QueryApi extends ApiClient.Api {


  /**
   * Test query parameter(s)
   * Test query parameter(s)
   * @param enumRefStringQuery  (optional)
   * @return String
   */
  @RequestLine("GET /query/enum_ref_string?enum_ref_string_query={enumRefStringQuery}")
  @Headers({
    "Accept: text/plain",
  })
  String testEnumRefString(@Param("enumRefStringQuery") StringEnumRef enumRefStringQuery);

  /**
   * Test query parameter(s)
   * Similar to <code>testEnumRefString</code> but it also returns the http response headers .
   * Test query parameter(s)
   * @param enumRefStringQuery  (optional)
   * @return A ApiResponse that wraps the response boyd and the http headers.
   */
  @RequestLine("GET /query/enum_ref_string?enum_ref_string_query={enumRefStringQuery}")
  @Headers({
    "Accept: text/plain",
  })
  ApiResponse<String> testEnumRefStringWithHttpInfo(@Param("enumRefStringQuery") StringEnumRef enumRefStringQuery);


  /**
   * Test query parameter(s)
   * Test query parameter(s)
   * Note, this is equivalent to the other <code>testEnumRefString</code> method,
   * but with the query parameters collected into a single Map parameter. This
   * is convenient for services with optional query parameters, especially when
   * used with the {@link TestEnumRefStringQueryParams} class that allows for
   * building up this map in a fluent style.
   * @param queryParams Map of query parameters as name-value pairs
   *   <p>The following elements may be specified in the query map:</p>
   *   <ul>
   *   <li>enumRefStringQuery -  (optional)</li>
   *   </ul>
   * @return String
   */
  @RequestLine("GET /query/enum_ref_string?enum_ref_string_query={enumRefStringQuery}")
  @Headers({
  "Accept: text/plain",
  })
  String testEnumRefString(@QueryMap(encoded=true) TestEnumRefStringQueryParams queryParams);

  /**
  * Test query parameter(s)
  * Test query parameter(s)
  * Note, this is equivalent to the other <code>testEnumRefString</code> that receives the query parameters as a map,
  * but this one also exposes the Http response headers
      * @param queryParams Map of query parameters as name-value pairs
      *   <p>The following elements may be specified in the query map:</p>
      *   <ul>
          *   <li>enumRefStringQuery -  (optional)</li>
      *   </ul>
          * @return String
      */
      @RequestLine("GET /query/enum_ref_string?enum_ref_string_query={enumRefStringQuery}")
      @Headers({
    "Accept: text/plain",
      })
   ApiResponse<String> testEnumRefStringWithHttpInfo(@QueryMap(encoded=true) TestEnumRefStringQueryParams queryParams);


   /**
   * A convenience class for generating query parameters for the
   * <code>testEnumRefString</code> method in a fluent style.
   */
  public static class TestEnumRefStringQueryParams extends HashMap<String, Object> {
    public TestEnumRefStringQueryParams enumRefStringQuery(final StringEnumRef value) {
      put("enum_ref_string_query", EncodingUtils.encode(value));
      return this;
    }
  }

  /**
   * Test query parameter(s)
   * Test query parameter(s)
   * @param datetimeQuery  (optional)
   * @param dateQuery  (optional)
   * @param stringQuery  (optional)
   * @return String
   */
  @RequestLine("GET /query/datetime/date/string?datetime_query={datetimeQuery}&date_query={dateQuery}&string_query={stringQuery}")
  @Headers({
    "Accept: text/plain",
  })
  String testQueryDatetimeDateString(@Param("datetimeQuery") OffsetDateTime datetimeQuery, @Param("dateQuery") LocalDate dateQuery, @Param("stringQuery") String stringQuery);

  /**
   * Test query parameter(s)
   * Similar to <code>testQueryDatetimeDateString</code> but it also returns the http response headers .
   * Test query parameter(s)
   * @param datetimeQuery  (optional)
   * @param dateQuery  (optional)
   * @param stringQuery  (optional)
   * @return A ApiResponse that wraps the response boyd and the http headers.
   */
  @RequestLine("GET /query/datetime/date/string?datetime_query={datetimeQuery}&date_query={dateQuery}&string_query={stringQuery}")
  @Headers({
    "Accept: text/plain",
  })
  ApiResponse<String> testQueryDatetimeDateStringWithHttpInfo(@Param("datetimeQuery") OffsetDateTime datetimeQuery, @Param("dateQuery") LocalDate dateQuery, @Param("stringQuery") String stringQuery);


  /**
   * Test query parameter(s)
   * Test query parameter(s)
   * Note, this is equivalent to the other <code>testQueryDatetimeDateString</code> method,
   * but with the query parameters collected into a single Map parameter. This
   * is convenient for services with optional query parameters, especially when
   * used with the {@link TestQueryDatetimeDateStringQueryParams} class that allows for
   * building up this map in a fluent style.
   * @param queryParams Map of query parameters as name-value pairs
   *   <p>The following elements may be specified in the query map:</p>
   *   <ul>
   *   <li>datetimeQuery -  (optional)</li>
   *   <li>dateQuery -  (optional)</li>
   *   <li>stringQuery -  (optional)</li>
   *   </ul>
   * @return String
   */
  @RequestLine("GET /query/datetime/date/string?datetime_query={datetimeQuery}&date_query={dateQuery}&string_query={stringQuery}")
  @Headers({
  "Accept: text/plain",
  })
  String testQueryDatetimeDateString(@QueryMap(encoded=true) TestQueryDatetimeDateStringQueryParams queryParams);

  /**
  * Test query parameter(s)
  * Test query parameter(s)
  * Note, this is equivalent to the other <code>testQueryDatetimeDateString</code> that receives the query parameters as a map,
  * but this one also exposes the Http response headers
      * @param queryParams Map of query parameters as name-value pairs
      *   <p>The following elements may be specified in the query map:</p>
      *   <ul>
          *   <li>datetimeQuery -  (optional)</li>
          *   <li>dateQuery -  (optional)</li>
          *   <li>stringQuery -  (optional)</li>
      *   </ul>
          * @return String
      */
      @RequestLine("GET /query/datetime/date/string?datetime_query={datetimeQuery}&date_query={dateQuery}&string_query={stringQuery}")
      @Headers({
    "Accept: text/plain",
      })
   ApiResponse<String> testQueryDatetimeDateStringWithHttpInfo(@QueryMap(encoded=true) TestQueryDatetimeDateStringQueryParams queryParams);


   /**
   * A convenience class for generating query parameters for the
   * <code>testQueryDatetimeDateString</code> method in a fluent style.
   */
  public static class TestQueryDatetimeDateStringQueryParams extends HashMap<String, Object> {
    public TestQueryDatetimeDateStringQueryParams datetimeQuery(final OffsetDateTime value) {
      put("datetime_query", EncodingUtils.encode(value));
      return this;
    }
    public TestQueryDatetimeDateStringQueryParams dateQuery(final LocalDate value) {
      put("date_query", EncodingUtils.encode(value));
      return this;
    }
    public TestQueryDatetimeDateStringQueryParams stringQuery(final String value) {
      put("string_query", EncodingUtils.encode(value));
      return this;
    }
  }

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
  String testQueryIntegerBooleanString(@QueryMap(encoded=true) TestQueryIntegerBooleanStringQueryParams queryParams);

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
   ApiResponse<String> testQueryIntegerBooleanStringWithHttpInfo(@QueryMap(encoded=true) TestQueryIntegerBooleanStringQueryParams queryParams);


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
  @RequestLine("GET /query/style_deepObject/explode_true/object?query_object={queryObject}")
  @Headers({
    "Accept: text/plain",
  })
  String testQueryStyleDeepObjectExplodeTrueObject(@Param("queryObject") Pet queryObject);

  /**
   * Test query parameter(s)
   * Similar to <code>testQueryStyleDeepObjectExplodeTrueObject</code> but it also returns the http response headers .
   * Test query parameter(s)
   * @param queryObject  (optional)
   * @return A ApiResponse that wraps the response boyd and the http headers.
   */
  @RequestLine("GET /query/style_deepObject/explode_true/object?query_object={queryObject}")
  @Headers({
    "Accept: text/plain",
  })
  ApiResponse<String> testQueryStyleDeepObjectExplodeTrueObjectWithHttpInfo(@Param("queryObject") Pet queryObject);


  /**
   * Test query parameter(s)
   * Test query parameter(s)
   * Note, this is equivalent to the other <code>testQueryStyleDeepObjectExplodeTrueObject</code> method,
   * but with the query parameters collected into a single Map parameter. This
   * is convenient for services with optional query parameters, especially when
   * used with the {@link TestQueryStyleDeepObjectExplodeTrueObjectQueryParams} class that allows for
   * building up this map in a fluent style.
   * @param queryParams Map of query parameters as name-value pairs
   *   <p>The following elements may be specified in the query map:</p>
   *   <ul>
   *   <li>queryObject -  (optional)</li>
   *   </ul>
   * @return String
   */
  @RequestLine("GET /query/style_deepObject/explode_true/object?query_object={queryObject}")
  @Headers({
  "Accept: text/plain",
  })
  String testQueryStyleDeepObjectExplodeTrueObject(@QueryMap(encoded=true) TestQueryStyleDeepObjectExplodeTrueObjectQueryParams queryParams);

  /**
  * Test query parameter(s)
  * Test query parameter(s)
  * Note, this is equivalent to the other <code>testQueryStyleDeepObjectExplodeTrueObject</code> that receives the query parameters as a map,
  * but this one also exposes the Http response headers
      * @param queryParams Map of query parameters as name-value pairs
      *   <p>The following elements may be specified in the query map:</p>
      *   <ul>
          *   <li>queryObject -  (optional)</li>
      *   </ul>
          * @return String
      */
      @RequestLine("GET /query/style_deepObject/explode_true/object?query_object={queryObject}")
      @Headers({
    "Accept: text/plain",
      })
   ApiResponse<String> testQueryStyleDeepObjectExplodeTrueObjectWithHttpInfo(@QueryMap(encoded=true) TestQueryStyleDeepObjectExplodeTrueObjectQueryParams queryParams);


   /**
   * A convenience class for generating query parameters for the
   * <code>testQueryStyleDeepObjectExplodeTrueObject</code> method in a fluent style.
   */
  public static class TestQueryStyleDeepObjectExplodeTrueObjectQueryParams extends HashMap<String, Object> {
    public TestQueryStyleDeepObjectExplodeTrueObjectQueryParams queryObject(final Pet value) {
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
  @RequestLine("GET /query/style_deepObject/explode_true/object/allOf?query_object={queryObject}")
  @Headers({
    "Accept: text/plain",
  })
  String testQueryStyleDeepObjectExplodeTrueObjectAllOf(@Param("queryObject") TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter queryObject);

  /**
   * Test query parameter(s)
   * Similar to <code>testQueryStyleDeepObjectExplodeTrueObjectAllOf</code> but it also returns the http response headers .
   * Test query parameter(s)
   * @param queryObject  (optional)
   * @return A ApiResponse that wraps the response boyd and the http headers.
   */
  @RequestLine("GET /query/style_deepObject/explode_true/object/allOf?query_object={queryObject}")
  @Headers({
    "Accept: text/plain",
  })
  ApiResponse<String> testQueryStyleDeepObjectExplodeTrueObjectAllOfWithHttpInfo(@Param("queryObject") TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter queryObject);


  /**
   * Test query parameter(s)
   * Test query parameter(s)
   * Note, this is equivalent to the other <code>testQueryStyleDeepObjectExplodeTrueObjectAllOf</code> method,
   * but with the query parameters collected into a single Map parameter. This
   * is convenient for services with optional query parameters, especially when
   * used with the {@link TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryParams} class that allows for
   * building up this map in a fluent style.
   * @param queryParams Map of query parameters as name-value pairs
   *   <p>The following elements may be specified in the query map:</p>
   *   <ul>
   *   <li>queryObject -  (optional)</li>
   *   </ul>
   * @return String
   */
  @RequestLine("GET /query/style_deepObject/explode_true/object/allOf?query_object={queryObject}")
  @Headers({
  "Accept: text/plain",
  })
  String testQueryStyleDeepObjectExplodeTrueObjectAllOf(@QueryMap(encoded=true) TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryParams queryParams);

  /**
  * Test query parameter(s)
  * Test query parameter(s)
  * Note, this is equivalent to the other <code>testQueryStyleDeepObjectExplodeTrueObjectAllOf</code> that receives the query parameters as a map,
  * but this one also exposes the Http response headers
      * @param queryParams Map of query parameters as name-value pairs
      *   <p>The following elements may be specified in the query map:</p>
      *   <ul>
          *   <li>queryObject -  (optional)</li>
      *   </ul>
          * @return String
      */
      @RequestLine("GET /query/style_deepObject/explode_true/object/allOf?query_object={queryObject}")
      @Headers({
    "Accept: text/plain",
      })
   ApiResponse<String> testQueryStyleDeepObjectExplodeTrueObjectAllOfWithHttpInfo(@QueryMap(encoded=true) TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryParams queryParams);


   /**
   * A convenience class for generating query parameters for the
   * <code>testQueryStyleDeepObjectExplodeTrueObjectAllOf</code> method in a fluent style.
   */
  public static class TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryParams extends HashMap<String, Object> {
    public TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryParams queryObject(final TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter value) {
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
  String testQueryStyleFormExplodeTrueArrayString(@QueryMap(encoded=true) TestQueryStyleFormExplodeTrueArrayStringQueryParams queryParams);

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
   ApiResponse<String> testQueryStyleFormExplodeTrueArrayStringWithHttpInfo(@QueryMap(encoded=true) TestQueryStyleFormExplodeTrueArrayStringQueryParams queryParams);


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
  String testQueryStyleFormExplodeTrueObject(@QueryMap(encoded=true) TestQueryStyleFormExplodeTrueObjectQueryParams queryParams);

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
   ApiResponse<String> testQueryStyleFormExplodeTrueObjectWithHttpInfo(@QueryMap(encoded=true) TestQueryStyleFormExplodeTrueObjectQueryParams queryParams);


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

  /**
   * Test query parameter(s)
   * Test query parameter(s)
   * @param queryObject  (optional)
   * @return String
   */
  @RequestLine("GET /query/style_form/explode_true/object/allOf?query_object={queryObject}")
  @Headers({
    "Accept: text/plain",
  })
  String testQueryStyleFormExplodeTrueObjectAllOf(@Param("queryObject") DataQuery queryObject);

  /**
   * Test query parameter(s)
   * Similar to <code>testQueryStyleFormExplodeTrueObjectAllOf</code> but it also returns the http response headers .
   * Test query parameter(s)
   * @param queryObject  (optional)
   * @return A ApiResponse that wraps the response boyd and the http headers.
   */
  @RequestLine("GET /query/style_form/explode_true/object/allOf?query_object={queryObject}")
  @Headers({
    "Accept: text/plain",
  })
  ApiResponse<String> testQueryStyleFormExplodeTrueObjectAllOfWithHttpInfo(@Param("queryObject") DataQuery queryObject);


  /**
   * Test query parameter(s)
   * Test query parameter(s)
   * Note, this is equivalent to the other <code>testQueryStyleFormExplodeTrueObjectAllOf</code> method,
   * but with the query parameters collected into a single Map parameter. This
   * is convenient for services with optional query parameters, especially when
   * used with the {@link TestQueryStyleFormExplodeTrueObjectAllOfQueryParams} class that allows for
   * building up this map in a fluent style.
   * @param queryParams Map of query parameters as name-value pairs
   *   <p>The following elements may be specified in the query map:</p>
   *   <ul>
   *   <li>queryObject -  (optional)</li>
   *   </ul>
   * @return String
   */
  @RequestLine("GET /query/style_form/explode_true/object/allOf?query_object={queryObject}")
  @Headers({
  "Accept: text/plain",
  })
  String testQueryStyleFormExplodeTrueObjectAllOf(@QueryMap(encoded=true) TestQueryStyleFormExplodeTrueObjectAllOfQueryParams queryParams);

  /**
  * Test query parameter(s)
  * Test query parameter(s)
  * Note, this is equivalent to the other <code>testQueryStyleFormExplodeTrueObjectAllOf</code> that receives the query parameters as a map,
  * but this one also exposes the Http response headers
      * @param queryParams Map of query parameters as name-value pairs
      *   <p>The following elements may be specified in the query map:</p>
      *   <ul>
          *   <li>queryObject -  (optional)</li>
      *   </ul>
          * @return String
      */
      @RequestLine("GET /query/style_form/explode_true/object/allOf?query_object={queryObject}")
      @Headers({
    "Accept: text/plain",
      })
   ApiResponse<String> testQueryStyleFormExplodeTrueObjectAllOfWithHttpInfo(@QueryMap(encoded=true) TestQueryStyleFormExplodeTrueObjectAllOfQueryParams queryParams);


   /**
   * A convenience class for generating query parameters for the
   * <code>testQueryStyleFormExplodeTrueObjectAllOf</code> method in a fluent style.
   */
  public static class TestQueryStyleFormExplodeTrueObjectAllOfQueryParams extends HashMap<String, Object> {
    public TestQueryStyleFormExplodeTrueObjectAllOfQueryParams queryObject(final DataQuery value) {
      put("query_object", EncodingUtils.encode(value));
      return this;
    }
  }
}
