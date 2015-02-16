/**
 *  Copyright 2015 Reverb, Inc.
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

package com.wordnik.swagger.codegen;

import com.wordnik.swagger.codegen.ClientOpts;
import com.wordnik.swagger.annotations.*;
import com.wordnik.swagger.models.Swagger;
import com.wordnik.swagger.models.auth.AuthorizationValue;

import java.util.*;
import java.net.URLEncoder;
import java.net.URLDecoder;

public class ClientOptInput {
  private ClientOpts opts;
  private Swagger swagger;
  private List<AuthorizationValue> auths;
  protected CodegenConfig config;

  public ClientOptInput swagger(Swagger swagger) {
    this.setSwagger(swagger);
    return this;
  }
  public ClientOptInput opts(ClientOpts opts) {
    this.setOpts(opts);
    return this;
  }

  public void setAuth(String urlEncodedAuthString) {
    List<AuthorizationValue> auths = new ArrayList<AuthorizationValue>();
    if(urlEncodedAuthString != null && !"".equals(urlEncodedAuthString)) {
      String[] parts = urlEncodedAuthString.split(",");
      for(String part : parts) {
        String[] kvPair = part.split(":");
        if(kvPair.length == 2) {
          auths.add(new AuthorizationValue(URLDecoder.decode(kvPair[0]), URLDecoder.decode(kvPair[1]), "header"));
        }
      }
    }
    this.auths = auths;
  }
  public String getAuth() {
    if(auths != null) {
      StringBuilder b = new StringBuilder();
      for(AuthorizationValue v : auths) {
        try {
          if(b.toString().length() > 0)
            b.append(",");
          b.append(URLEncoder.encode(v.getKeyName(), "UTF-8"))
            .append(":")
            .append(URLEncoder.encode(v.getValue(), "UTF-8"));
        }
        catch (Exception e) {
          // continue
          e.printStackTrace();
        }  
      }
      return b.toString();
    }
    else
      return null;
  }
  public List<AuthorizationValue> getAuthorizationValues() {
    return auths;
  }

  public CodegenConfig getConfig() {
    return config;
  }
  public void setConfig(CodegenConfig config) {
    this.config = config;
  }

  public void setOpts(ClientOpts opts) {
    this.opts = opts;
  }

  public ClientOpts getOpts() {
    return opts;
  }

  public void setSwagger(Swagger swagger) {
    this.swagger = swagger;
  }

  @ApiModelProperty(dataType="Object")
  public Swagger getSwagger() {
    return swagger;
  }
}