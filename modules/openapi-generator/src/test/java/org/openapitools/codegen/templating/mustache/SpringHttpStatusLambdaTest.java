package org.openapitools.codegen.templating.mustache;

import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.util.Locale;
import java.util.Map;

import static org.openapitools.codegen.templating.mustache.SpringHttpStatusLambda.HTTP_STATUS_PREFIX;
import static org.testng.Assert.assertThrows;

public class SpringHttpStatusLambdaTest extends LambdaTest {

  @DataProvider(name = "springHttpStatusCodes")
  public static Object[][] springHttpStatusCodes() {
    return new Object[][] {
      {"ACCEPTED", "202"},
      {"ALREADY_REPORTED", "208"},
      {"BAD_GATEWAY", "502"},
      {"BAD_REQUEST", "400"},
      {"BANDWIDTH_LIMIT_EXCEEDED", "509"},
      {"CONFLICT", "409"},
      {"CONTINUE", "100"},
      {"CREATED", "201"},
      {"EARLY_HINTS", "103"},
      {"EXPECTATION_FAILED", "417"},
      {"FAILED_DEPENDENCY", "424"},
      {"FORBIDDEN", "403"},
      {"FOUND", "302"},
      {"GATEWAY_TIMEOUT", "504"},
      {"GONE", "410"},
      {"HTTP_VERSION_NOT_SUPPORTED", "505"},
      {"I_AM_A_TEAPOT", "418"},
      {"IM_USED", "226"},
      {"INSUFFICIENT_STORAGE", "507"},
      {"INTERNAL_SERVER_ERROR", "500"},
      {"LENGTH_REQUIRED", "411"},
      {"LOCKED", "423"},
      {"LOOP_DETECTED", "508"},
      {"METHOD_NOT_ALLOWED", "405"},
      {"MOVED_PERMANENTLY", "301"},
      {"MULTI_STATUS", "207"},
      {"MULTIPLE_CHOICES", "300"},
      {"NETWORK_AUTHENTICATION_REQUIRED", "511"},
      {"NO_CONTENT", "204"},
      {"NON_AUTHORITATIVE_INFORMATION", "203"},
      {"NOT_ACCEPTABLE", "406"},
      {"NOT_EXTENDED", "510"},
      {"NOT_FOUND", "404"},
      {"NOT_IMPLEMENTED", "501"},
      {"NOT_MODIFIED", "304"},
      {"OK", "200"},
      {"PARTIAL_CONTENT", "206"},
      {"PAYLOAD_TOO_LARGE", "413"},
      {"PAYMENT_REQUIRED", "402"},
      {"PERMANENT_REDIRECT", "308"},
      {"PRECONDITION_FAILED", "412"},
      {"PRECONDITION_REQUIRED", "428"},
      {"PROCESSING", "102"},
      {"PROXY_AUTHENTICATION_REQUIRED", "407"},
      {"REQUEST_HEADER_FIELDS_TOO_LARGE", "431"},
      {"REQUEST_TIMEOUT", "408"},
      {"REQUESTED_RANGE_NOT_SATISFIABLE", "416"},
      {"RESET_CONTENT", "205"},
      {"SEE_OTHER", "303"},
      {"SERVICE_UNAVAILABLE", "503"},
      {"SWITCHING_PROTOCOLS", "101"},
      {"TEMPORARY_REDIRECT", "307"},
      {"TOO_EARLY", "425"},
      {"TOO_MANY_REQUESTS", "429"},
      {"UNAUTHORIZED", "401"},
      {"UNAVAILABLE_FOR_LEGAL_REASONS", "451"},
      {"UNPROCESSABLE_ENTITY", "422"},
      {"UNSUPPORTED_MEDIA_TYPE", "415"},
      {"UPGRADE_REQUIRED", "426"},
      {"URI_TOO_LONG", "414"},
      {"VARIANT_ALSO_NEGOTIATES", "506"}
    };
  }

  @Test(dataProvider = "springHttpStatusCodes")
  public void executeShouldConvertValues(String expectedHttpStatus, String httpCode) {
    Map<String, Object> ctx = context("springHttpStatus", new SpringHttpStatusLambda());

    test(
        HTTP_STATUS_PREFIX + expectedHttpStatus,
        String.format(Locale.ROOT, "{{#springHttpStatus}}%s{{/springHttpStatus}}", httpCode),
        ctx);
  }

  @Test
  public void executeShouldConvertEmptyHttpStatusTo200Ok() {
    Map<String, Object> ctx = context("springHttpStatus", new SpringHttpStatusLambda());

    test(HTTP_STATUS_PREFIX + "OK", "{{#springHttpStatus}}{{/springHttpStatus}}", ctx);
  }

  @Test
  public void executeShouldConvertToHttpStatusNotImplementedAnyOtherStatus() {
    Map<String, Object> ctx = context("springHttpStatus", new SpringHttpStatusLambda());

    assertThrows(
        IllegalArgumentException.class,
        () -> execute("{{#springHttpStatus}}305{{/springHttpStatus}}", ctx));
  }
}
