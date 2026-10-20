package org.openapitools.codegen.utils;

import lombok.Data;
import org.testng.annotations.Test;

import java.util.List;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;

public class YamlConfigUtilsTest {

    @Test
    public void testLoadConfigAsMap() {
        Map<String, YamlConfig> yamlConfig = YamlConfigUtils.loadAsMap("yamlConfigMap.yaml", YamlConfig.class);
        assertThat(yamlConfig.keySet()).containsOnly("firstItem", "secondItem");

        YamlConfig firstItem = yamlConfig.get("firstItem");
        assertThat(firstItem.getStringConfig()).isEqualTo("Hello");
        assertThat(firstItem.getNumberConfig()).isEqualTo(42);
        assertThat(firstItem.getObjectConfig().getAttribut()).isEqualTo("openapi");
        assertThat(firstItem.getArrayConfig()).hasSize(2);
        assertThat(firstItem.getArrayConfig().get(0)).isEqualTo("one");
        assertThat(firstItem.getArrayConfig().get(1)).isEqualTo("two");

        YamlConfig secondItem = yamlConfig.get("secondItem");
        assertThat(secondItem.getStringConfig()).isEqualTo("World");
        assertThat(secondItem.getNumberConfig()).isNull();
        assertThat(secondItem.getObjectConfig()).isNull();
        assertThat(secondItem.getArrayConfig()).isNull();
    }

    @Data
    static class YamlConfig {
        private String stringConfig;
        private Integer numberConfig;
        private ObjectConfig objectConfig;
        private List<String> arrayConfig;
    }

    @Data
    static class ObjectConfig {
        private String attribut;
    }
}