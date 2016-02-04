package io.swagger.api.impl;

import io.swagger.api.*;
import io.swagger.model.*;




import java.util.List;
import io.swagger.api.NotFoundException;

import java.io.InputStream;

import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;

@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JavaResteasyServerCodegen", date = "2016-02-04T01:58:20.368+07:00")

public class PettestingByteArraytrueApiServiceImpl extends PettestingByteArraytrueApiService {
  
      @Override
      public Response addPetUsingByteArray(byte[] body,SecurityContext securityContext)
      throws NotFoundException {
      // do some magic!
      return Response.ok().entity(new ApiResponseMessage(ApiResponseMessage.OK, "magic!")).build();
  }
  
}

