/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
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

package org.openapitools.codegen.auth;

import io.swagger.v3.parser.core.models.AuthorizationValue;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.List;

import static org.apache.commons.lang3.StringUtils.isNotEmpty;

public class AuthParser {

    private static final Logger LOGGER = LoggerFactory.getLogger(AuthParser.class);

    public static List<AuthorizationValue> parse(String urlEncodedAuthStr) {
        List<AuthorizationValue> auths = new ArrayList<AuthorizationValue>();
        if (isNotEmpty(urlEncodedAuthStr)) {
            String[] parts = urlEncodedAuthStr.split(",");
            for (String part : parts) {
                String[] kvPair = part.split(":");
                if (kvPair.length == 2) {
                    try {
                        auths.add(new AuthorizationValue(URLDecoder.decode(kvPair[0], "UTF-8"), URLDecoder.decode(kvPair[1], "UTF-8"), "header"));
                    } catch (UnsupportedEncodingException e) {
                        LOGGER.warn(e.getMessage());
                    }
                }
            }
        }
        return auths;
    }

    public static String reconstruct(List<AuthorizationValue> authorizationValueList) {
        if (authorizationValueList != null) {
            StringBuilder b = new StringBuilder();
            for (AuthorizationValue v : authorizationValueList) {
                try {
                    if (b.toString().length() > 0) {
                        b.append(",");
                    }
                    b.append(URLEncoder.encode(v.getKeyName(), "UTF-8"))
                            .append(":")
                            .append(URLEncoder.encode(v.getValue(), "UTF-8"));
                } catch (Exception e) {
                    // continue
                    LOGGER.error(e.getMessage(), e);
                }
            }
            return b.toString();
        } else {
            return null;
        }
    }
}
