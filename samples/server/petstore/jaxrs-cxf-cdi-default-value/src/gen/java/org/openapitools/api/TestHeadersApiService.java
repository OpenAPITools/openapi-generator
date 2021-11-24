package org.openapitools.api;

import org.openapitools.api.*;
import org.openapitools.model.*;

import org.apache.cxf.jaxrs.ext.multipart.Attachment;
import org.apache.cxf.jaxrs.ext.multipart.Multipart;

import java.math.BigDecimal;
import org.openapitools.model.TestResponse;

import java.util.List;

import java.io.InputStream;

import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSCXFCDIServerCodegen")
public interface TestHeadersApiService {
      public Response headersTest(BigDecimal headerNumber, String headerString, String headerStringWrapped, String headerStringQuotes, String headerStringQuotesWrapped, Boolean headerBoolean, SecurityContext securityContext);
}
