package controllers;

import java.math.BigDecimal;
import apimodels.Client;
import apimodels.FileSchemaTestClass;
import java.io.InputStream;
import java.time.LocalDate;
import java.util.Map;
import java.time.OffsetDateTime;
import apimodels.OuterComposite;
import apimodels.User;

import play.mvc.Http;
import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;

import javax.validation.constraints.*;

@SuppressWarnings("RedundantThrows")
public interface FakeApiControllerImpInterface {
    Boolean fakeOuterBooleanSerialize(Boolean body) throws Exception;

    OuterComposite fakeOuterCompositeSerialize(OuterComposite outerComposite) throws Exception;

    BigDecimal fakeOuterNumberSerialize(BigDecimal body) throws Exception;

    String fakeOuterStringSerialize(String body) throws Exception;

    void testBodyWithFileSchema(FileSchemaTestClass fileSchemaTestClass) throws Exception;

    void testBodyWithQueryParams( @NotNull String query, User user) throws Exception;

    Client testClientModel(Client client) throws Exception;

    void testEndpointParameters(BigDecimal number, Double _double, String patternWithoutDelimiter, byte[] _byte, Integer integer, Integer int32, Long int64, Float _float, String string, Http.MultipartFormData.FilePart binary, LocalDate date, OffsetDateTime dateTime, String password, String paramCallback) throws Exception;

    void testEnumParameters(List<String> enumHeaderStringArray, String enumHeaderString, List<String> enumQueryStringArray, String enumQueryString, Integer enumQueryInteger, Double enumQueryDouble, List<String> enumFormStringArray, String enumFormString) throws Exception;

    void testInlineAdditionalProperties(Map<String, String> requestBody) throws Exception;

    void testJsonFormData(String param, String param2) throws Exception;

}
