package org.openapitools.api;

import org.openapitools.api.*;
import org.openapitools.model.*;

import org.wso2.msf4j.formparam.FormDataParam;
import org.wso2.msf4j.formparam.FileInfo;

import java.math.BigDecimal;
import org.openapitools.model.Client;
import java.util.Date;
import java.io.File;
import org.openapitools.model.FileSchemaTestClass;
import java.util.Map;
import org.openapitools.model.ModelApiResponse;
import org.openapitools.model.OuterComposite;
import org.openapitools.model.User;
import org.openapitools.model.XmlItem;

import java.util.List;
import org.openapitools.api.NotFoundException;

import java.io.InputStream;

import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaMSF4JServerCodegen")
public abstract class FakeApiService {
    public abstract Response createXmlItem(XmlItem xmlItem
 ) throws NotFoundException;
    public abstract Response fakeOuterBooleanSerialize(Boolean body
 ) throws NotFoundException;
    public abstract Response fakeOuterCompositeSerialize(OuterComposite body
 ) throws NotFoundException;
    public abstract Response fakeOuterNumberSerialize(BigDecimal body
 ) throws NotFoundException;
    public abstract Response fakeOuterStringSerialize(String body
 ) throws NotFoundException;
    public abstract Response testBodyWithFileSchema(FileSchemaTestClass body
 ) throws NotFoundException;
    public abstract Response testBodyWithQueryParams(String query
 ,User body
 ) throws NotFoundException;
    public abstract Response testClientModel(Client body
 ) throws NotFoundException;
    public abstract Response testEndpointParameters(BigDecimal number
 ,Double _double
 ,String patternWithoutDelimiter
 ,byte[] _byte
 ,Integer integer
 ,Integer int32
 ,Long int64
 ,Float _float
 ,String string
 ,InputStream binaryInputStream, FileInfo binaryDetail
 ,Date date
 ,Date dateTime
 ,String password
 ,String paramCallback
 ) throws NotFoundException;
    public abstract Response testEnumParameters(List<String> enumHeaderStringArray
 ,String enumHeaderString
 ,List<String> enumQueryStringArray
 ,String enumQueryString
 ,Integer enumQueryInteger
 ,Double enumQueryDouble
 ,List<String> enumFormStringArray
 ,String enumFormString
 ) throws NotFoundException;
    public abstract Response testGroupParameters(Integer requiredStringGroup
 ,Boolean requiredBooleanGroup
 ,Long requiredInt64Group
 ,Integer stringGroup
 ,Boolean booleanGroup
 ,Long int64Group
 ) throws NotFoundException;
    public abstract Response testInlineAdditionalProperties(Map<String, String> param
 ) throws NotFoundException;
    public abstract Response testJsonFormData(String param
 ,String param2
 ) throws NotFoundException;
    public abstract Response testQueryParameterCollectionFormat(List<String> pipe
 ,List<String> ioutil
 ,List<String> http
 ,List<String> url
 ,List<String> context
 ) throws NotFoundException;
    public abstract Response uploadFileWithRequiredFile(Long petId
 ,InputStream requiredFileInputStream, FileInfo requiredFileDetail
 ,String additionalMetadata
 ) throws NotFoundException;
}
