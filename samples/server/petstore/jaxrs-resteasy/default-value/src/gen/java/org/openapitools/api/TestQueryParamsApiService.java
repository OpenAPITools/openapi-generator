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
public interface TestQueryParamsApiService {
      Response queryParamsTest(BigDecimal queryNumber,String queryString,String queryStringWrapped,String queryStringQuotes,String queryStringQuotesWrapped,Boolean queryBoolean,SecurityContext securityContext)
      throws NotFoundException;
}
