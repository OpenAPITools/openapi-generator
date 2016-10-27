package io.swagger.codegen.config;

import io.swagger.codegen.utils.OptionUtils;
import org.apache.commons.lang3.tuple.Pair;

import java.util.*;

/**
 * Contains shared logic for applying key-value pairs and CSV strings
 * to specific settings in CodegenConfigurator.
 *
 * <p>
 *     This class exists to facilitate testing. These methods could be applied
 *     to CodegenConfigurator, but this complicates things when mocking CodegenConfigurator.
 * </p>
 */
public final class CodegenConfiguratorUtils {

    public static void applySystemPropertiesKvp(String systemProperties, CodegenConfigurator configurator) {
        final Map<String, String> map = createMapFromKeyValuePairs(systemProperties);
        for (Map.Entry<String, String> entry : map.entrySet()) {
            configurator.addSystemProperty(entry.getKey(), entry.getValue());
        }
    }

    public static void applyInstantiationTypesKvp(String instantiationTypes, CodegenConfigurator configurator) {
        final Map<String, String> map = createMapFromKeyValuePairs(instantiationTypes);
        for (Map.Entry<String, String> entry : map.entrySet()) {
            configurator.addInstantiationType(entry.getKey(), entry.getValue());
        }
    }

    public static void applyImportMappingsKvp(String importMappings, CodegenConfigurator configurator) {
        final Map<String, String> map = createMapFromKeyValuePairs(importMappings);
        for (Map.Entry<String, String> entry : map.entrySet()) {
            configurator.addImportMapping(entry.getKey().trim(), entry.getValue().trim());
        }
    }

    public static void applyTypeMappingsKvp(String typeMappings, CodegenConfigurator configurator) {
        final Map<String, String> map = createMapFromKeyValuePairs(typeMappings);
        for (Map.Entry<String, String> entry : map.entrySet()) {
            configurator.addTypeMapping(entry.getKey(), entry.getValue());
        }
    }

    public static void applyAdditionalPropertiesKvp(String additionalProperties, CodegenConfigurator configurator) {
        final Map<String, String> map = createMapFromKeyValuePairs(additionalProperties);
        for (Map.Entry<String, String> entry : map.entrySet()) {
            configurator.addAdditionalProperty(entry.getKey(), entry.getValue());
        }
    }

    public static void applyLanguageSpecificPrimitivesCsv(String languageSpecificPrimitives, CodegenConfigurator configurator) {
        final Set<String> set = createSetFromCsvList(languageSpecificPrimitives);
        for (String item : set) {
            configurator.addLanguageSpecificPrimitive(item);
        }
    }

    private static Set<String> createSetFromCsvList(String csvProperty) {
        final List<String> values = OptionUtils.splitCommaSeparatedList(csvProperty);
        return new HashSet<String>(values);
    }

    private static Map<String, String> createMapFromKeyValuePairs(String commaSeparatedKVPairs) {
        final List<Pair<String, String>> pairs = OptionUtils.parseCommaSeparatedTuples(commaSeparatedKVPairs);

        Map<String, String> result = new HashMap<String, String>();

        for (Pair<String, String> pair : pairs) {
            result.put(pair.getLeft(), pair.getRight());
        }

        return result;
    }
}
