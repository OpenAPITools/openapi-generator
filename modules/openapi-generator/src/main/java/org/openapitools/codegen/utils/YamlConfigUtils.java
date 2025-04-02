package org.openapitools.codegen.utils;

import org.yaml.snakeyaml.LoaderOptions;
import org.yaml.snakeyaml.TypeDescription;
import org.yaml.snakeyaml.Yaml;
import org.yaml.snakeyaml.constructor.Constructor;
import org.yaml.snakeyaml.introspector.BeanAccess;
import org.yaml.snakeyaml.nodes.MappingNode;
import org.yaml.snakeyaml.nodes.Node;
import org.yaml.snakeyaml.nodes.Tag;

import java.io.InputStream;
import java.util.Map;
import java.util.stream.Collectors;

public class YamlConfigUtils {

    /**
     * Load yaml config file as map
     *
     * @param configFile yaml config file to load
     * @param clazz      class of object to map data
     * @param <T>        type of config object to generate
     * @return config object generated
     */
    public static <T> Map<String, T> loadAsMap(String configFile, Class<T> clazz) {
        LoaderOptions loaderOptions = new LoaderOptions();
        loaderOptions.setAllowDuplicateKeys(false);

        Yaml yaml = new Yaml(new MapConstructor(loaderOptions, clazz));
        yaml.setBeanAccess(BeanAccess.FIELD);
        InputStream inputStream = YamlConfigUtils.class
                .getClassLoader()
                .getResourceAsStream(configFile);

        return yaml.load(inputStream);
    }

    private static class MapConstructor extends Constructor {
        private final TypeDescription itemType;

        public <T> MapConstructor(LoaderOptions loaderOptions, Class<T> clazz) {
            super(loaderOptions);
            this.rootTag = new Tag("root");
            itemType = new TypeDescription(clazz);
            this.addTypeDescription(itemType);
        }

        @Override
        protected Object constructObject(Node node) {
            if ("root".equals(node.getTag().getValue()) && node instanceof MappingNode) {
                MappingNode mNode = (MappingNode) node;
                return mNode.getValue().stream().collect(
                        Collectors.toMap(
                                t -> super.constructObject(t.getKeyNode()),
                                t -> {
                                    Node child = t.getValueNode();
                                    child.setType(itemType.getType());
                                    return super.constructObject(child);
                                }
                        )
                );

            } else {
                return super.constructObject(node);
            }
        }
    }
}