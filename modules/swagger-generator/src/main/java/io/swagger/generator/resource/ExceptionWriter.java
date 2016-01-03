package io.swagger.generator.resource;

import io.swagger.generator.exception.ApiException;
import io.swagger.generator.exception.BadRequestException;
import io.swagger.generator.exception.NotFoundException;
import io.swagger.generator.model.ApiResponse;
import io.swagger.generator.util.ValidationException;

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
            return Response
                    .status(e.getResponse().getStatus())
                    .entity(new ApiResponse(e.getResponse().getStatus(),
                            exception.getMessage())).build();
        } else if (exception instanceof com.fasterxml.jackson.core.JsonParseException) {
            return Response.status(400)
                    .entity(new ApiResponse(400, "bad input")).build();
        } else if (exception instanceof ValidationException) {
            ValidationException e = (ValidationException) exception;
            return Response.status(Status.BAD_REQUEST).entity(e.getMessage()).build();
        } else if (exception instanceof NotFoundException) {
            return Response
                    .status(Status.NOT_FOUND)
                    .entity(new ApiResponse(ApiResponse.ERROR, exception
                            .getMessage())).build();
        } else if (exception instanceof BadRequestException) {
            return Response
                    .status(Status.BAD_REQUEST)
                    .entity(new ApiResponse(ApiResponse.ERROR, exception
                            .getMessage())).build();
        } else if (exception instanceof ApiException) {
            return Response
                    .status(Status.BAD_REQUEST)
                    .entity(new ApiResponse(ApiResponse.ERROR, exception
                            .getMessage())).build();
        } else {
            return Response.status(500)
                    .entity(new ApiResponse(500, "something bad happened"))
                    .build();
        }
    }
}