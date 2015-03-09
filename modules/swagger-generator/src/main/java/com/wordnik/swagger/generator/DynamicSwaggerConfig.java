package com.wordnik.swagger.generator;

import com.wordnik.swagger.models.*;
import com.wordnik.swagger.models.parameters.*;
import com.wordnik.swagger.models.properties.*;
import com.wordnik.swagger.codegen.*;

import com.wordnik.swagger.util.Json;

import com.wordnik.swagger.jaxrs.config.BeanConfig;

import java.util.*;

public class DynamicSwaggerConfig extends BeanConfig {
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

  @Override
  public Swagger configure(Swagger swagger) {
    Path clientPath = swagger.getPaths().get("/gen/clients/{language}");
    // update the path description based on what clients are available via SPI
    if(clientPath != null) {
      Operation post = clientPath.getPost();
      Parameter framework = post.getParameters().get(0);
      if(framework instanceof PathParameter) {
        PathParameter param = (PathParameter) framework;
        StringBuilder b = new StringBuilder();
        for(String client : clients) {
          if(b.toString().length() > 0)
            b.append(", ");
          b.append(client);
        }
        param.setDescription("available clients: " + b.toString());
      }
    }

    Path serverPath = swagger.getPaths().get("/gen/servers/{framework}");
    // update the path description based on what servers are available via SPI
    if(serverPath != null) {
      Operation post = serverPath.getPost();
      Parameter framework = post.getParameters().get(0);
      if(framework instanceof PathParameter) {
        PathParameter param = (PathParameter) framework;
        StringBuilder b = new StringBuilder();
        for(String server : servers) {
          if(b.toString().length() > 0)
            b.append(", ");
          b.append(server);
        }
        param.setDescription("available clients: " + b.toString());
      }
    }

    return swagger.info(getInfo())
      .host(getHost())
      .basePath("/api");
  }
}