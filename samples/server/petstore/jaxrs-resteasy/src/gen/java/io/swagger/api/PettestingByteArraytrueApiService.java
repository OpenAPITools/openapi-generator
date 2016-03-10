package io.swagger.api;

import io.swagger.api.*;
import io.swagger.model.*;



import java.util.List;
import io.swagger.api.NotFoundException;

import java.io.InputStream;

import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;

@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JavaResteasyServerCodegen", date = "2016-03-10T11:00:42.854+08:00")
public abstract class PettestingByteArraytrueApiService {
  
      public abstract Response addPetUsingByteArray(byte[] body,SecurityContext securityContext)
      throws NotFoundException;
  
}
