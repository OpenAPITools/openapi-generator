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

package com.wordnik.swagger.generator;

import com.wordnik.swagger.models.*;

import javax.servlet.http.HttpServlet;
import javax.servlet.ServletContext;
import javax.servlet.ServletConfig;
import javax.servlet.ServletException;

public class Bootstrap extends HttpServlet {
  public void init(ServletConfig config) throws ServletException {
    ServletContext context = config.getServletContext();

    DynamicSwaggerConfig bc = new DynamicSwaggerConfig();
    bc.setBasePath("/api");
    bc.setTitle("Swagger Generator");
    bc.setDescription("This is an online swagger codegen server.  You can find out more " + 
      "at <a href=\"https://github.com/wordnik/swagger-generator\">https://github.com/swagger-api/swagger-codegen</a> or on irc.freenode.net, #swagger." +
      "http://helloreverb.com/terms/");
    bc.setTermsOfServiceUrl("http://helloreverb.com/terms/");
    bc.setContact("apiteam@swagger.io");
    bc.setLicense("Apache 2.0");
    bc.setVersion("1.0.0");
    bc.setHost("generator.swagger.io");
    bc.setLicenseUrl("http://www.apache.org/licenses/LICENSE-2.0.html");
    bc.setResourcePackage("com.wordnik.swagger.generator.resource");
    bc.setScan(true);
  }
}
