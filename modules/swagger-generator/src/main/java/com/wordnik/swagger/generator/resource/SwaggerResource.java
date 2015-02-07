/**
 *  Copyright 2014 Reverb, Inc.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package com.wordnik.swagger.generator.resource;

import com.wordnik.swagger.codegen.*;
import com.wordnik.swagger.generator.util.*;
import com.wordnik.swagger.annotations.*;
import com.wordnik.swagger.generator.model.*;
import com.wordnik.swagger.generator.exception.BadRequestException;
import com.wordnik.swagger.online.Generator;

import java.io.File;
import java.util.*;

import javax.ws.rs.*;
import javax.ws.rs.core.*;

@Path("/gen")
@Api(value = "/gen", description = "Resource for generating swagger components")
public class SwaggerResource {
  private static Map<String, Generated> fileMap = new HashMap<String, Generated>();

  @GET
  @Path("/download/{fileId}")
  @Produces({"application/zip", "application/json"})
  public Response downloadFile(@PathParam("fileId") String fileId) throws Exception {
    Generated g = fileMap.get(fileId);
    System.out.println("looking for fileId " + fileId);
    System.out.println("got filename " + g.getFilename());
    if(g.getFilename() != null) {
      byte[] bytes = org.apache.commons.io.FileUtils.readFileToByteArray(new java.io.File(g.getFilename()));

      return Response.ok(bytes, "application/zip")
        .header("Content-Disposition","attachment; filename=\"" + g.getFriendlyName() + "-generated.zip\"")
        .header("Accept-Range", "bytes")
        .header("Content-Length", bytes.length)
        .build();
    }
    else {
      return Response.status(404).build();
    }
  }

  @POST
  @Path("/clients/{language}")
  @Produces({"application/zip", "application/json"})
  @ApiOperation(value = "Generates a client library based on the config",
    notes = "The model representing this is not accurate, it needs to contain a consolidated JSON structure")
  public Response generateClient(
    @ApiParam(value = "The target language for the client library", allowableValues = "android,java,php,objc,docs", required = true) @PathParam("language") String language,
    @ApiParam(value = "Configuration for building the client library", required = true) GeneratorInput opts) throws Exception {

    String filename = Generator.generateClient(language, opts);

    if(filename != null) {
      String code = String.valueOf(System.currentTimeMillis());
      Generated g = new Generated();
      g.setFilename(filename);
      g.setFriendlyName(language + "-client");
      fileMap.put(code, g);
      System.out.println(code + ", " + filename);
      return Response.ok().entity(new ResponseCode(code)).build();
    }
    else {
      return Response.status(500).build();
    }
  }

  @GET
  @Path("/clients")
  @ApiOperation(value = "Gets languages supported by the client generator",
    response = String.class,
    responseContainer = "List")
  public Response clientOptions() {
    String[] languages = {"android", "java", "php", "objc", "docs"};
    return Response.ok().entity(languages).build();
  }

  @GET
  @Path("/clients/{language}")
  @ApiOperation(value = "Gets options for a client generation",
    notes = "Values which are not required will use the provided default values",
    response = InputOption.class,
    responseContainer = "List")
  @ApiResponses(value = {
    @com.wordnik.swagger.annotations.ApiResponse(code = 400, message = "Invalid model supplied", response = ValidationMessage.class),
  })

  public Response clientLibraryOptions(
    @ApiParam(value = "The target language for the client library", allowableValues = "android,java,php,objc,docs", required = true) @PathParam("language") String language) {
    return Response.ok().entity(Generator.clientOptions(language)).build();
  }

  @GET
  @Path("/servers")
  @ApiOperation(value = "Gets languages supported by the server generator",
    response = String.class,
    responseContainer = "List")
  public Response serverOptions() {
    String[] languages = {"jaxrs","nodejs"};
    return Response.ok().entity(languages).build();
  }

  @GET
  @Path("/servers/{language}")
  @ApiOperation(value = "Gets options for a server generation",
    notes = "Values which are not required will use the provided default values",
    response = InputOption.class,
    responseContainer = "List")
  public Response serverFrameworkOptions(
    @ApiParam(value = "The target framework for the client library", allowableValues = "jaxrs,nodejs", required = true) @PathParam("language") String framework) {
    return Response.ok().entity(Generator.serverOptions(framework)).build();
  }

  @POST
  @Path("/servers/{framework}")
  @ApiOperation(value = "Generates a server library for the supplied server framework",
    notes = "The model representing this is not accurate, it needs to contain a consolidated JSON structure")
  public Response generateServerForLanguage(
    @ApiParam(value = "framework", allowableValues = "jaxrs,nodejs", required = true) @PathParam("framework") String framework,
    @ApiParam(value = "parameters", required = true) GeneratorInput opts)
      throws Exception {
    if(framework == null)
      throw new BadRequestException(400, "Framework is required");
    String filename = Generator.generateServer(framework, opts);
    System.out.println("generated name: " + filename);

    if(filename != null) {
      String code = String.valueOf(System.currentTimeMillis());
      Generated g = new Generated();
      g.setFilename(filename);
      g.setFriendlyName(framework + "-server");
      fileMap.put(code, g);
      System.out.println(code + ", " + filename);
      return Response.ok().entity(new ResponseCode(code)).build();
    }
    else {
      return Response.status(500).build();
    }
  }
}
