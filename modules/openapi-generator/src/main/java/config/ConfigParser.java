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

package config;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.Iterator;
import java.util.Map;

public class ConfigParser {

    private static final Logger LOGGER = LoggerFactory.getLogger(ConfigParser.class);

    public static Config read(String location) {

        LOGGER.info("reading config from " + location);

        ObjectMapper mapper = new ObjectMapper();

        Config config = new Config();

        try {
            JsonNode rootNode = mapper.readTree(new File(location));
            Iterator<Map.Entry<String, JsonNode>> optionNodes = rootNode.fields();

            while (optionNodes.hasNext()) {
                Map.Entry<String, JsonNode> optionNode = optionNodes.next();

                if (optionNode.getValue().isValueNode()) {
                    config.setOption(optionNode.getKey(), optionNode.getValue().asText());
                } else {
                    LOGGER.warn("omitting non-value node " + optionNode.getKey());
                }
            }
        } catch (Exception e) {
            LOGGER.error(e.getMessage());
            return null;
        }

        return config;
    }
}
