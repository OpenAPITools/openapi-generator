package org.openapitools.api;

import org.openapitools.api.*;
import org.openapitools.model.*;

import org.glassfish.jersey.media.multipart.FormDataBodyPart;

import java.math.BigDecimal;
import org.openapitools.model.Client;
import java.util.Date;
import java.io.File;
import org.openapitools.model.FileSchemaTestClass;
import java.util.Map;
import org.openapitools.model.OuterComposite;
import org.openapitools.model.User;
import org.openapitools.model.XmlItem;

import java.util.List;
import org.openapitools.api.NotFoundException;

import java.io.InputStream;

import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;
import javax.validation.constraints.*;
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJerseyServerCodegen")
public abstract class FakeApiService {
    public abstract Response createXmlItem(XmlItem xmlItem,SecurityContext securityContext) throws NotFoundException;
    public abstract Response fakeOuterBooleanSerialize(Boolean body,SecurityContext securityContext) throws NotFoundException;
    public abstract Response fakeOuterCompositeSerialize(OuterComposite body,SecurityContext securityContext) throws NotFoundException;
    public abstract Response fakeOuterNumberSerialize(BigDecimal body,SecurityContext securityContext) throws NotFoundException;
    public abstract Response fakeOuterStringSerialize(String body,SecurityContext securityContext) throws NotFoundException;
    public abstract Response testBodyWithFileSchema(FileSchemaTestClass body,SecurityContext securityContext) throws NotFoundException;
    public abstract Response testBodyWithQueryParams( @NotNull String query,User body,SecurityContext securityContext) throws NotFoundException;
    public abstract Response testClientModel(Client body,SecurityContext securityContext) throws NotFoundException;
    public abstract Response testEndpointParameters(BigDecimal number,Double _double,String patternWithoutDelimiter,byte[] _byte,Integer integer,Integer int32,Long int64,Float _float,String string,FormDataBodyPart binaryBodypart,Date date,Date dateTime,String password,String paramCallback,SecurityContext securityContext) throws NotFoundException;
    public abstract Response testEnumParameters(List<String> enumHeaderStringArray,String enumHeaderString,List<String> enumQueryStringArray,String enumQueryString,Integer enumQueryInteger,Double enumQueryDouble,List<String> enumFormStringArray,String enumFormString,SecurityContext securityContext) throws NotFoundException;
    public abstract Response testGroupParameters( @NotNull Integer requiredStringGroup, @NotNull Boolean requiredBooleanGroup, @NotNull Long requiredInt64Group,Integer stringGroup,Boolean booleanGroup,Long int64Group,SecurityContext securityContext) throws NotFoundException;
    public abstract Response testInlineAdditionalProperties(Map<String, String> param,SecurityContext securityContext) throws NotFoundException;
    public abstract Response testJsonFormData(String param,String param2,SecurityContext securityContext) throws NotFoundException;
    public abstract Response testQueryParameterCollectionFormat( @NotNull List<String> pipe, @NotNull List<String> ioutil, @NotNull List<String> http, @NotNull List<String> url, @NotNull List<String> context,SecurityContext securityContext) throws NotFoundException;
}
