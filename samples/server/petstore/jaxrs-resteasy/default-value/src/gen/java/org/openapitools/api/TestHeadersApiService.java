package org.openapitools.api;

import org.openapitools.api.*;
import org.openapitools.model.*;


import java.math.BigDecimal;
import org.openapitools.model.TestResponse;

import java.util.List;
import org.openapitools.api.NotFoundException;

import java.io.InputStream;

import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaResteasyServerCodegen")
public interface TestHeadersApiService {
      Response headersTest(BigDecimal headerNumber,String headerString,String headerStringWrapped,String headerStringQuotes,String headerStringQuotesWrapped,Boolean headerBoolean,SecurityContext securityContext)
      throws NotFoundException;
}
