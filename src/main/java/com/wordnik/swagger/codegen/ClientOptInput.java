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

package com.wordnik.swagger.codegen;

import com.wordnik.swagger.codegen.ClientOpts;

import com.wordnik.swagger.models.Swagger;

public class ClientOptInput {
  private ClientOpts opts;
  private Swagger swagger;
  protected CodegenConfig config;

  public ClientOptInput swagger(Swagger swagger) {
    this.setSwagger(swagger);
    return this;
  }
  public ClientOptInput opts(ClientOpts opts) {
    this.setOpts(opts);
    return this;
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

  public Swagger getSwagger() {
    return swagger;
  }
}