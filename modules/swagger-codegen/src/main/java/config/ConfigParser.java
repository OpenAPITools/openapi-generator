package config;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.io.File;
import java.util.Iterator;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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
