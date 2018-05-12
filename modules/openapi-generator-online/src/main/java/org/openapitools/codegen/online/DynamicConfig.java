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

import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.CodegenConfigLoader;
import org.openapitools.codegen.CodegenType;
import io.swagger.jaxrs.config.BeanConfig;
import io.swagger.models.Operation;
import io.swagger.models.Path;
import io.swagger.models.Swagger;
import io.swagger.models.parameters.Parameter;
import io.swagger.models.parameters.PathParameter;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class DynamicConfig extends BeanConfig {
    static List<String> clients = new ArrayList<String>();
    static List<String> servers = new ArrayList<String>();

    static {
        List<CodegenConfig> extensions = CodegenConfigLoader.getAll();
        for (CodegenConfig config : extensions) {
            if (config.getTag().equals(CodegenType.CLIENT)
                    || config.getTag().equals(CodegenType.DOCUMENTATION)) {
                clients.add(config.getName());
            } else if (config.getTag().equals(CodegenType.SERVER)) {
                servers.add(config.getName());
            }
        }
        Collections.sort(clients, String.CASE_INSENSITIVE_ORDER);
        Collections.sort(servers, String.CASE_INSENSITIVE_ORDER);
    }

    @Override
    public Swagger configure(Swagger swagger) {
        Path clientPath = swagger.getPaths().get("/gen/clients/{language}");
        // update the path description based on what clients are available via SPI
        if (clientPath != null) {
            Operation post = clientPath.getPost();
            Parameter framework = post.getParameters().get(0);
            if (framework instanceof PathParameter) {
                PathParameter param = (PathParameter) framework;
                param.setEnum(clients);
            }

            Operation get = clientPath.getGet();
            if (get != null) {
                framework = get.getParameters().get(0);
                if (framework instanceof PathParameter) {
                    PathParameter param = (PathParameter) framework;
                    param.setEnum(clients);
                }
            }
        }

        Path serverPath = swagger.getPaths().get("/gen/servers/{framework}");
        if (serverPath != null) {
            Operation post = serverPath.getPost();
            Parameter framework = post.getParameters().get(0);
            if (framework instanceof PathParameter) {
                PathParameter param = (PathParameter) framework;
                param.setEnum(servers);
            }

            Operation get = serverPath.getGet();
            if (get != null) {
                framework = get.getParameters().get(0);
                if (framework instanceof PathParameter) {
                    PathParameter param = (PathParameter) framework;
                    param.setEnum(servers);
                }
            }
        }

        return swagger.info(getInfo()).host(getHost()).basePath("/api");
    }
}
