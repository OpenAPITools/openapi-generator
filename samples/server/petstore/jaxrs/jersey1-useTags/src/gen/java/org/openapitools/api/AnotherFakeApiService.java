package org.openapitools.api;

import org.openapitools.api.*;
import org.openapitools.model.*;

import com.sun.jersey.multipart.FormDataParam;

import org.openapitools.model.Client;

import java.util.List;
import org.openapitools.api.NotFoundException;

import java.io.InputStream;

import com.sun.jersey.core.header.FormDataContentDisposition;
import com.sun.jersey.multipart.FormDataParam;

import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;
import javax.validation.constraints.*;

public abstract class AnotherFakeApiService {
      public abstract Response call123testSpecialTags(Client body,SecurityContext securityContext)
      throws NotFoundException;
}
