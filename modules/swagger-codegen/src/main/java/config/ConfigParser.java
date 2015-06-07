package config;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.io.File;
import java.util.Iterator;
import java.util.Map;

public class ConfigParser {

    public static Config read(String location) {

        System.out.println("reading config from " + location);

        ObjectMapper mapper = new ObjectMapper();

        Config config = new Config();

        try {
            JsonNode rootNode = mapper.readTree(new File(location));
            Iterator<Map.Entry<String, JsonNode>> optionNodes = rootNode.fields();

            while (optionNodes.hasNext()) {
                Map.Entry<String, JsonNode> optionNode = (Map.Entry<String, JsonNode>) optionNodes.next();

                if (optionNode.getValue().isValueNode()) {
                    config.setOption(optionNode.getKey(), optionNode.getValue().asText());
                } else {
                    System.out.println("omitting non-value node " + optionNode.getKey());
                }
            }
        } catch (Exception e) {
            System.out.println(e.getMessage());
            return null;
        }

        return config;
    }
}
