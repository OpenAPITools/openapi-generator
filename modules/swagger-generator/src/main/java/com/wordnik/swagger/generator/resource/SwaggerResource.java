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

  static List<String> clients = new ArrayList<String>();
  static List<String> servers = new ArrayList<String>();
  static {
    List<CodegenConfig> extensions = Codegen.getExtensions();
    for(CodegenConfig config : extensions) {
      if(config.getTag().equals(CodegenType.CLIENT) || config.getTag().equals(CodegenType.DOCUMENTATION)) {
        clients.add(config.getName());
      }
      else if(config.getTag().equals(CodegenType.SERVER)) {
        servers.add(config.getName());
      }
    }
  }

  @GET
  @Path("/download/{fileId}")
  @Produces({MediaType.APPLICATION_OCTET_STREAM})
  @ApiOperation(value = "Downloads a pre-generated file",
    response = String.class,
    tags = {"clients", "servers"})
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
  @ApiOperation(
    value = "Generates a client library based on the config",
    response = ResponseCode.class,
    tags = "clients")
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
      String link = "http://generator.swagger.io/api/gen/download/" + code;
      return Response.ok().entity(new ResponseCode(code, link)).build();
    }
    else {
      return Response.status(500).build();
    }
  }

  @GET
  @Path("/clients")
  @ApiOperation(value = "Gets languages supported by the client generator",
    response = String.class,
    responseContainer = "List",
    tags = "clients")
  public Response clientOptions() {
    String[] languages = new String[clients.size()];
    languages = clients.toArray(languages);
    return Response.ok().entity(languages).build();
  }

  @GET
  @Path("/servers")
  @ApiOperation(value = "Gets languages supported by the server generator",
    response = String.class,
    responseContainer = "List",
    tags = "servers")
  public Response serverOptions() {
    String[] languages = new String[servers.size()];
    languages = servers.toArray(languages);
    return Response.ok().entity(languages).build();
  }

  @POST
  @Path("/servers/{framework}")
  @ApiOperation(value = "Generates a server library for the supplied server framework",
    response = ResponseCode.class,
    tags = "servers")
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
      String link = "http://generator.swagger.io/api/gen/download/" + code;
      return Response.ok().entity(new ResponseCode(code, link)).build();
    }
    else {
      return Response.status(500).build();
    }
  }
}
