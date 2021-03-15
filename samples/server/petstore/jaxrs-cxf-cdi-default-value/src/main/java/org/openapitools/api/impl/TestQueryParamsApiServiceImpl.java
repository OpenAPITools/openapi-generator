package org.openapitools.api.impl;

import org.openapitools.api.*;
import org.openapitools.model.*;

import org.apache.cxf.jaxrs.ext.multipart.Attachment;

import java.math.BigDecimal;
import org.openapitools.model.TestResponse;

import java.util.List;

import java.io.InputStream;

import javax.enterprise.context.RequestScoped;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;

@RequestScoped
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSCXFCDIServerCodegen")
public class TestQueryParamsApiServiceImpl implements TestQueryParamsApiService {
      @Override
      public Response queryParamsTest(BigDecimal queryNumber, String queryString, String queryStringWrapped, String queryStringQuotes, String queryStringQuotesWrapped, Boolean queryBoolean, SecurityContext securityContext) {
      // do some magic!
      return Response.ok().entity("magic!").build();
  }
}
