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

package org.openapitools.codegen.online;

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
        DynamicConfig bc = new DynamicConfig();
        bc.setBasePath("/api");
        bc.setTitle("OpenAPI Generator");
        bc.setDescription("This is an online OpenAPI generator.  You can find out more "
                + "at https://github.com/openapi-tools/openapi-generator");
        bc.setTermsOfServiceUrl("https://www.apache.org/licenses/LICENSE-2.0");
        bc.setContact("openapi-generator@gmail.com");
        bc.setLicense("Apache 2.0");
        InputStream stream = getClass().getResourceAsStream("/version.prop");
        if (stream == null) {
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
        bc.setResourcePackage("org.openapitools.codegen.online.resource");
        bc.setScan(true);
    }
}
