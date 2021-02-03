package org.openapitools.client.api;

import com.github.tomakehurst.wiremock.WireMockServer;
import com.github.tomakehurst.wiremock.common.Slf4jNotifier;
import feign.FeignException;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.openapitools.client.ApiClient;
import org.openapitools.client.model.Client;
import org.openapitools.client.model.OuterComposite;
import org.openapitools.client.model.User;
import org.openapitools.client.model.XmlItem;
import org.threeten.bp.LocalDate;
import org.threeten.bp.OffsetDateTime;

import java.io.File;
import java.io.IOException;
import java.math.BigDecimal;
import java.util.Arrays;
import java.util.HashMap;

import static com.github.tomakehurst.wiremock.client.WireMock.*;
import static com.github.tomakehurst.wiremock.core.WireMockConfiguration.options;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

class FakeApiTest {

  private static WireMockServer wm = new WireMockServer(options().dynamicPort().notifier(new Slf4jNotifier(true)));

  private FakeApi api;

  @BeforeAll
  static void startWireMock() {
    wm.start();
  }

  @AfterAll
  static void stopWireMock() {
    wm.shutdown();
  }

  @BeforeEach
  void setUp() {
    ApiClient apiClient = new ApiClient();
    apiClient.setBasePath(wm.baseUrl());
    api = apiClient.buildClient(FakeApi.class);
  }

  @Test
  void createXmlItem() {
    wm.stubFor(post(urlEqualTo("/fake/create_xml_item"))
            .withHeader("Content-Type", equalTo("application/xml"))
            .withHeader("Accept", equalTo("application/json"))
            .willReturn(ok()));

    XmlItem xmlItem = new XmlItem();
    api.createXmlItem(xmlItem);
  }

  @ParameterizedTest
  @ValueSource(strings = {"true", "false"})
  void fakeOuterBooleanSerialize(String returnBoolean) {
    wm.stubFor(post(urlEqualTo("/fake/outer/boolean"))
            .withHeader("Content-Type", equalTo("*/*"))
            .withHeader("Accept", equalTo("*/*"))
            .willReturn(ok(returnBoolean)));

    boolean expectedBoolean = Boolean.parseBoolean(returnBoolean);
    Boolean aBoolean = api.fakeOuterBooleanSerialize(expectedBoolean);

    assertThat(aBoolean, is(expectedBoolean));
  }

  @Test
  void fakeOuterCompositeSerialize() {
    String body = "{\n" +
            "   \"my_number\": 123.45,\n" +
            "   \"my_string\":\"Hello\",\n" +
            "   \"my_boolean\": true\n" +
            "}";
    wm.stubFor(post(urlEqualTo("/fake/outer/composite"))
            .withHeader("Content-Type", equalTo("*/*"))
            .withHeader("Accept", equalTo("*/*"))
            .withRequestBody(equalToJson(body, true, false))
            .willReturn(ok(body)));

    OuterComposite outerComposite = new OuterComposite();
    outerComposite.setMyBoolean(Boolean.TRUE);
    outerComposite.setMyNumber(BigDecimal.valueOf(123.45));
    outerComposite.setMyString("Hello");

    OuterComposite result = api.fakeOuterCompositeSerialize(outerComposite);

    assertThat(result, is(outerComposite));
  }

  @Test
  void fakeOuterNumberSerialize() {
    String body = "123.45";
    wm.stubFor(post(urlEqualTo("/fake/outer/number"))
            .withHeader("Content-Type", equalTo("*/*"))
            .withHeader("Accept", equalTo("*/*"))
            .withRequestBody(equalTo(body))
            .willReturn(ok(body)));

    BigDecimal result = api.fakeOuterNumberSerialize(BigDecimal.valueOf(123.45));

    assertThat(result, is(result));
  }

  @Test
  void fakeOuterStringSerialize() {
    String body = "Hello world";
    wm.stubFor(post(urlEqualTo("/fake/outer/string"))
            .withHeader("Content-Type", equalTo("*/*"))
            .withHeader("Accept", equalTo("*/*"))
            .withRequestBody(equalTo("\"" + body + "\""))
            .willReturn(ok("\"" + body + "\"")));

    String result = api.fakeOuterStringSerialize(body);

    assertThat(result, is(body));
  }

  @Test
  void testBodyWithFileSchema() throws IOException {
    //TODO
  }

  @Test
  void testBodyWithQueryParams() {
    String body = "{\n" +
            "   \"id\":123456,\n" +
            "   \"username\":null,\n" +
            "   \"firstName\":\"Bruce\",\n" +
            "   \"lastName\":\"Wayne\",\n" +
            "   \"email\":\"mail@email.com\",\n" +
            "   \"password\":\"password\",\n" +
            "   \"phone\":\"+123 3313131\",\n" +
            "   \"userStatus\":1\n" +
            "}";

    wm.stubFor(put(urlEqualTo("/fake/body-with-query-params?query=tags"))
            .withHeader("Content-Type", equalTo("application/json"))
            .withHeader("Accept", equalTo("application/json"))
            .withRequestBody(equalToJson(body))
            .willReturn(ok()));

    User user = new User();
    user.setEmail("mail@email.com");
    user.setFirstName("Bruce");
    user.setLastName("Wayne");
    user.setId(123456L);
    user.setUserStatus(1);
    user.setPassword("password");
    user.setPhone("+123 3313131");

    api.testBodyWithQueryParams("tags", user);
  }

  @Test
  void testBodyWithQueryParamsMap() {
    String body = "{\n" +
            "   \"id\":123456,\n" +
            "   \"username\":null,\n" +
            "   \"firstName\":\"Bruce\",\n" +
            "   \"lastName\":\"Wayne\",\n" +
            "   \"email\":\"mail@email.com\",\n" +
            "   \"password\":\"password\",\n" +
            "   \"phone\":\"+123 3313131\",\n" +
            "   \"userStatus\":1\n" +
            "}";

    wm.stubFor(put(urlEqualTo("/fake/body-with-query-params?query=value1"))
            .withHeader("Content-Type", equalTo("application/json"))
            .withHeader("Accept", equalTo("application/json"))
            .withRequestBody(equalToJson(body))
            .willReturn(ok()));

    User user = new User();
    user.setEmail("mail@email.com");
    user.setFirstName("Bruce");
    user.setLastName("Wayne");
    user.setId(123456L);
    user.setUserStatus(1);
    user.setPassword("password");
    user.setPhone("+123 3313131");

    FakeApi.TestBodyWithQueryParamsQueryParams queryParams = new FakeApi.TestBodyWithQueryParamsQueryParams();
    queryParams.query("value1");

    api.testBodyWithQueryParams(user, queryParams);
  }

  @Test
  void testClientModel() {
    String body = "{\n" +
            "   \"client\":\"Mr Wayne\"\n" +
            "}";

    wm.stubFor(patch(urlEqualTo("/fake"))
            .withHeader("Content-Type", equalTo("application/json"))
            .withHeader("Accept", equalTo("application/json"))
            .withRequestBody(equalToJson(body))
            .willReturn(ok(body)));

    Client client = new Client();
    client.setClient("Mr Wayne");

    Client result = api.testClientModel(client);

    assertThat(result.getClient(), is("Mr Wayne"));
  }

  @Test
  void testEndpointParameters() throws IOException {
    wm.stubFor(post(urlEqualTo("/fake"))
            .withHeader("Content-Type", containing("application/x-www-form-urlencoded"))
            .withHeader("Accept", equalTo("application/json"))
            .willReturn(ok()));

    //TODO Cannot serialize bytearray to x-www-form-urlencoded, must use multipart
    api.testEndpointParameters(BigDecimal.ONE, 1.0, "abc", null, 123,
            1234, 123L, 1.0f, "string", File.createTempFile("testEndpointParameters", "tmp"), LocalDate.now(), OffsetDateTime.now(),
            "password", "callback");
  }

  @Test
  void testEnumParameters() {
    //TODO GET method does not allow request body
  }

  @Test
  void testGroupParameters() {
    wm.stubFor(delete(urlEqualTo("/fake?required_string_group=123&required_int64_group=123&string_group=123&int64_group=123"))
            .withHeader("Accept", equalTo("application/json"))
            .withHeader("required_boolean_group", equalTo("true"))
            .withHeader("boolean_group", equalTo("true"))
            .willReturn(ok()));

    api.testGroupParameters(123, true, 123L, 123, true, 123L);
  }

  @Test
  void testInlineAdditionalProperties() {

    wm.stubFor(post(urlEqualTo("/fake/inline-additionalProperties"))
            .withHeader("Content-Type", equalTo("application/json"))
            .withHeader("Accept", equalTo("application/json"))
            .willReturn(ok()));

    api.testInlineAdditionalProperties(new HashMap<>());

  }

  @Test
  void testQueryParameterCollectionFormat() {

    wm.stubFor(put(urlEqualTo("/fake/test-query-paramters?pipe=pipe1&pipe=pipe2&ioutil=io&http=http&url=url&context=context"))
            .withHeader("Accept", equalTo("application/json"))
            .willReturn(ok()));

    api.testQueryParameterCollectionFormat(Arrays.asList("pipe1", "pipe2"), Arrays.asList("io"), Arrays.asList("http"), Arrays.asList("url"), Arrays.asList("context"));
  }

  @Test
  void testQueryParameterCollectionFormatQueryParams() {

    wm.stubFor(put(urlEqualTo("/fake/test-query-paramters?ioutil=io&context=context&http=http&pipe=pipe1&pipe=pipe2&url=url"))
            .withHeader("Accept", equalTo("application/json"))
            .willReturn(ok()));

    HashMap<String, Object> params = new HashMap<>();
    params.put("context", Arrays.asList("context"));
    params.put("pipe", Arrays.asList("pipe1", "pipe2"));
    params.put("ioutil", Arrays.asList("io"));
    params.put("http", Arrays.asList("http"));
    params.put("url", Arrays.asList("url"));

    api.testQueryParameterCollectionFormat(params);
  }

  @Test
  void test404() {
    wm.stubFor(post(urlEqualTo("/fake/create_xml_item"))
            .withHeader("Content-Type", equalTo("application/xml"))
            .withHeader("Accept", equalTo("application/json"))
            .willReturn(notFound()));

    XmlItem xmlItem = new XmlItem();
    assertThrows(FeignException.NotFound.class, () -> api.createXmlItem(xmlItem));
  }

  @Test
  void test500() {
    wm.stubFor(post(urlEqualTo("/fake/create_xml_item"))
            .withHeader("Content-Type", equalTo("application/xml"))
            .withHeader("Accept", equalTo("application/json"))
            .willReturn(serverError()));

    XmlItem xmlItem = new XmlItem();
    assertThrows(FeignException.InternalServerError.class, () -> api.createXmlItem(xmlItem));
  }

  @Test
  void test400() {
    wm.stubFor(post(urlEqualTo("/fake/create_xml_item"))
            .withHeader("Content-Type", equalTo("application/xml"))
            .withHeader("Accept", equalTo("application/json"))
            .willReturn(badRequest()));

    XmlItem xmlItem = new XmlItem();
    assertThrows(FeignException.BadRequest.class, () -> api.createXmlItem(xmlItem));
  }
}