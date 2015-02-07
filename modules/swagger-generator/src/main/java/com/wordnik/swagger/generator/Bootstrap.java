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

package com.wordnik.swagger.generator;

import com.wordnik.swagger.models.*;

import javax.servlet.http.HttpServlet;
import javax.servlet.ServletContext;
import javax.servlet.ServletConfig;
import javax.servlet.ServletException;

public class Bootstrap extends HttpServlet {
  public void init(ServletConfig config) throws ServletException {
    Info info = new Info()
      .title("Swagger Generator")
      .description("This is an online swagger codegen server.  You can find out more " + 
      "at <a href=\"https://github.com/wordnik/swagger-generator\">https://github.com/wordnik/swagger-generator</a> or on irc.freenode.net, #swagger." +
      "http://helloreverb.com/terms/")
      .termsOfService("http://helloreverb.com/terms/")
      .contact(new Contact()
        .email("apiteam@swagger.io"))
      .license(new License()
        .name("Apache 2.0")
        .url("http://www.apache.org/licenses/LICENSE-2.0.html"));

    ServletContext context = config.getServletContext();
    Swagger swagger = new Swagger().info(info);
    context.setAttribute("swagger", swagger);
  }
}
