/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.online.resource;

import org.openapitools.codegen.online.exception.ApiException;
import org.openapitools.codegen.online.exception.BadRequestException;
import org.openapitools.codegen.online.exception.NotFoundException;
import org.openapitools.codegen.online.model.ApiResponse;
import org.openapitools.codegen.online.util.ValidationException;

import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;
import javax.ws.rs.ext.ExceptionMapper;
import javax.ws.rs.ext.Provider;

@Provider
public class ExceptionWriter implements ExceptionMapper<Exception> {
    @Override
    public Response toResponse(Exception exception) {
        if (exception instanceof javax.ws.rs.WebApplicationException) {
            javax.ws.rs.WebApplicationException e = (javax.ws.rs.WebApplicationException) exception;
            return Response.status(e.getResponse().getStatus())
                    .entity(new ApiResponse(e.getResponse().getStatus(), exception.getMessage()))
                    .build();
        } else if (exception instanceof com.fasterxml.jackson.core.JsonParseException) {
            return Response.status(400).entity(new ApiResponse(400, "bad input")).build();
        } else if (exception instanceof ValidationException) {
            ValidationException e = (ValidationException) exception;
            return Response.status(Status.BAD_REQUEST).entity(e.getMessage()).build();
        } else if (exception instanceof NotFoundException) {
            return Response.status(Status.NOT_FOUND)
                    .entity(new ApiResponse(ApiResponse.ERROR, exception.getMessage())).build();
        } else if (exception instanceof BadRequestException) {
            return Response.status(Status.BAD_REQUEST)
                    .entity(new ApiResponse(ApiResponse.ERROR, exception.getMessage())).build();
        } else if (exception instanceof ApiException) {
            return Response.status(Status.BAD_REQUEST)
                    .entity(new ApiResponse(ApiResponse.ERROR, exception.getMessage())).build();
        } else {
            return Response.status(500).entity(new ApiResponse(500, "something bad happened"))
                    .build();
        }
    }
}
