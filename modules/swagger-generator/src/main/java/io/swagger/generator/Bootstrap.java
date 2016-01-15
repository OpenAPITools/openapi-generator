/**
 * Copyright 2016 SmartBear Software
 * <p>
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * <p>
 * http://www.apache.org/licenses/LICENSE-2.0
 * <p>
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.swagger.generator;

import org.apache.commons.io.IOUtils;

import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import java.io.IOException;
import java.io.InputStream;

public class Bootstrap extends HttpServlet {
    private static final long serialVersionUID = 1400930071893332856L;

    @Override
    public void init(ServletConfig config) throws ServletException {
        DynamicSwaggerConfig bc = new DynamicSwaggerConfig();
        bc.setBasePath("/api");
        bc.setTitle("Swagger Generator");
        bc.setDescription("This is an online swagger codegen server.  You can find out more " +
                "at https://github.com/swagger-api/swagger-codegen or on [irc.freenode.net, #swagger](http://swagger.io/irc/).");
        bc.setTermsOfServiceUrl("http://swagger.io/terms/");
        bc.setContact("apiteam@swagger.io");
        bc.setLicense("Apache 2.0");
        InputStream stream = getClass().getResourceAsStream("/version.prop");
        if(stream == null) {
            bc.setVersion("0.0.0");
        } else {
            try {
                bc.setVersion(IOUtils.toString(stream, "UTF-8"));
                stream.close();
            } catch (IOException e) {
                bc.setVersion("0.0.0");
            }
        }

        bc.setLicenseUrl("http://www.apache.org/licenses/LICENSE-2.0.html");
        bc.setResourcePackage("io.swagger.generator.resource");
        bc.setScan(true);
    }
}
