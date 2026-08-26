/*
 * Copyright 2026 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.openapitools.codegen.languages;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.security.SecurityRequirement;
import io.swagger.v3.oas.models.security.SecurityScheme;
import io.swagger.v3.oas.models.servers.Server;
import org.openapitools.codegen.CodegenOperation;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Direction-agnostic operation facts shared by the Boost.Beast client and
 * server template assemblers: raw-operation lookup, effective security
 * groups, and server-list classification.
 */
final class CppBoostBeastOperationFacts {
    private CppBoostBeastOperationFacts() {
    }

    /** True when the list is exactly swagger-parser's implicit root default
     *  (a single Server with url "/") and the raw source omitted {@code servers}. */
    static boolean isParserDefaultServerList(List<Server> servers) {
        return servers != null && servers.size() == 1
                && "/".equals(servers.get(0).getUrl());
    }

    /** The operation's effective security requirements as template-ready
     *  groups. Each group is an OR alternative containing AND-required scheme
     *  maps. An empty group is anonymous access; operation {@code security: []}
     *  clears inherited requirements. */
    static List<List<Map<String, Object>>> effectiveSecurityGroups(
            OpenAPI document, CodegenOperation op) {
        List<List<Map<String, Object>>> groups = new ArrayList<>();
        List<SecurityRequirement> requirements = null;
        io.swagger.v3.oas.models.Operation raw = operationFor(document, op);
        if (raw != null && raw.getSecurity() != null) {
            requirements = raw.getSecurity();        // includes `[]` clears
        } else if (document != null
                && document.getSecurity() != null) {
            requirements = document.getSecurity();
        }
        if (requirements == null) {
            return groups;                            // no security declared
        }
        Map<String, SecurityScheme> schemes = document != null
                && document.getComponents() != null
                ? document.getComponents().getSecuritySchemes()
                : null;
        for (SecurityRequirement req : requirements) {
            List<Map<String, Object>> ands = new ArrayList<>();
            if (req != null) {
                for (Map.Entry<String, List<String>> e : req.entrySet()) {
                    SecurityScheme scheme = schemes == null
                            ? null : schemes.get(e.getKey());
                    Map<String, Object> use = new LinkedHashMap<>();
                    use.put("name", cppString(e.getKey()));
                    use.put("type", cppString(scheme == null || scheme.getType() == null
                            ? "unknown" : scheme.getType().toString()));
                    if (scheme != null && scheme.getType() == SecurityScheme.Type.APIKEY) {
                        use.put("in", cppString(scheme.getIn() == null ? "header"
                                : scheme.getIn().toString()));
                        use.put("paramName", cppString(scheme.getName() == null
                                ? "" : scheme.getName()));
                    } else {
                        use.put("in", "");
                        use.put("paramName", "");
                    }
                    use.put("httpScheme", cppString(scheme != null
                            && scheme.getType() == SecurityScheme.Type.HTTP
                            && scheme.getScheme() != null
                            ? scheme.getScheme() : ""));
                    List<String> scopes = e.getValue() == null
                            ? new ArrayList<String>() : e.getValue();
                    use.put("scopes", scopes);
                    use.put("scopesRendered", scopes.isEmpty() ? null
                            : scopes.stream()
                                    .map(s -> "\"" + cppString(s) + "\"")
                                    .collect(java.util.stream.Collectors
                                            .joining(", ")));
                    ands.add(use);
                }
            }
            groups.add(ands);                          // empty ands = {}
        }
        return groups;
    }

    /** The raw Operation behind a CodegenOperation (PathItem-method lookup). */
    static io.swagger.v3.oas.models.Operation operationFor(
            OpenAPI document, CodegenOperation op) {
        if (document == null || document.getPaths() == null) {
            return null;
        }
        PathItem item = document.getPaths().get(op.path);
        if (item == null) {
            return null;
        }
        if ("GET".equals(op.httpMethod)) {
            return item.getGet();
        }
        if ("PUT".equals(op.httpMethod)) {
            return item.getPut();
        }
        if ("POST".equals(op.httpMethod)) {
            return item.getPost();
        }
        if ("DELETE".equals(op.httpMethod)) {
            return item.getDelete();
        }
        if ("OPTIONS".equals(op.httpMethod)) {
            return item.getOptions();
        }
        if ("HEAD".equals(op.httpMethod)) {
            return item.getHead();
        }
        if ("PATCH".equals(op.httpMethod)) {
            return item.getPatch();
        }
        if ("TRACE".equals(op.httpMethod)) {
            return item.getTrace();
        }
        return null;
    }

    private static String cppString(String value) {
        return CppBoostBeastModelCodegen.escapeCppStringContent(
                value == null ? "" : value);
    }
}
