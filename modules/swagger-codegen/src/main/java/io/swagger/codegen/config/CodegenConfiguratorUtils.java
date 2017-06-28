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
 * <ul>
 *     <li>The methods named {@code apply...Kvp} take a string of comma-separated key-value pairs.</li>
 *     <li>The methods named {@code apply...KvpList} take a list of such strings.</li>
 *     <li>The method named {@code apply...Csv} takes a string of comma-separated values.</li>
 *     <li>The method named {@code apply...CsvList} takes a list of such strings.</li>
 * </ul>
 * <p>
 *     The corresponding {@code add...} method on the passed configurator is called for each key-value pair (or value).
 * </p>  
 */
public final class CodegenConfiguratorUtils {

    public static void applySystemPropertiesKvpList(List<String> systemProperties, CodegenConfigurator configurator) {
        for(String propString : systemProperties) {
            applySystemPropertiesKvp(propString, configurator);
        }
    }

    public static void applySystemPropertiesKvp(String systemProperties, CodegenConfigurator configurator) {
        final Map<String, String> map = createMapFromKeyValuePairs(systemProperties);
        for (Map.Entry<String, String> entry : map.entrySet()) {
            configurator.addSystemProperty(entry.getKey(), entry.getValue());
        }
    }

    public static void applyInstantiationTypesKvpList(List<String> instantiationTypes, CodegenConfigurator configurator) {
        for(String propString : instantiationTypes) {
            applyInstantiationTypesKvp(propString, configurator);
        }
    }
    
    public static void applyInstantiationTypesKvp(String instantiationTypes, CodegenConfigurator configurator) {
        final Map<String, String> map = createMapFromKeyValuePairs(instantiationTypes);
        for (Map.Entry<String, String> entry : map.entrySet()) {
            configurator.addInstantiationType(entry.getKey(), entry.getValue());
        }
    }

    public static void applyImportMappingsKvpList(List<String> importMappings, CodegenConfigurator configurator) {
        for(String propString : importMappings) {
            applyImportMappingsKvp(propString, configurator);
        }
    }

    public static void applyImportMappingsKvp(String importMappings, CodegenConfigurator configurator) {
        final Map<String, String> map = createMapFromKeyValuePairs(importMappings);
        for (Map.Entry<String, String> entry : map.entrySet()) {
            configurator.addImportMapping(entry.getKey().trim(), entry.getValue().trim());
        }
    }

    public static void applyTypeMappingsKvpList(List<String> typeMappings, CodegenConfigurator configurator) {
        for(String propString : typeMappings) {
            applyTypeMappingsKvp(propString, configurator);
        }
    }

    public static void applyTypeMappingsKvp(String typeMappings, CodegenConfigurator configurator) {
        final Map<String, String> map = createMapFromKeyValuePairs(typeMappings);
        for (Map.Entry<String, String> entry : map.entrySet()) {
            configurator.addTypeMapping(entry.getKey(), entry.getValue());
        }
    }

    public static void applyAdditionalPropertiesKvpList(List<String> additionalProperties, CodegenConfigurator configurator) {
        for(String propString : additionalProperties) {
            applyAdditionalPropertiesKvp(propString, configurator);
        }
    }

    public static void applyAdditionalPropertiesKvp(String additionalProperties, CodegenConfigurator configurator) {
        final Map<String, String> map = createMapFromKeyValuePairs(additionalProperties);
        for (Map.Entry<String, String> entry : map.entrySet()) {
            configurator.addAdditionalProperty(entry.getKey(), entry.getValue());
        }
    }

    public static void applyLanguageSpecificPrimitivesCsvList(List<String> languageSpecificPrimitives, CodegenConfigurator configurator) {
        for(String propString : languageSpecificPrimitives) {
            applyLanguageSpecificPrimitivesCsv(propString, configurator);
        }
    }

    public static void applyLanguageSpecificPrimitivesCsv(String languageSpecificPrimitives, CodegenConfigurator configurator) {
        final Set<String> set = createSetFromCsvList(languageSpecificPrimitives);
        for (String item : set) {
            configurator.addLanguageSpecificPrimitive(item);
        }
    }

    public static void applyReservedWordsMappingsKvpList(List<String> reservedWordMappings, CodegenConfigurator configurator) {
        for(String propString : reservedWordMappings) {
            applyReservedWordsMappingsKvp(propString, configurator);
        }
    }

    public static void applyReservedWordsMappingsKvp(String reservedWordMappings, CodegenConfigurator configurator) {
        final Map<String, String> map = createMapFromKeyValuePairs(reservedWordMappings);
        for (Map.Entry<String, String> entry : map.entrySet()) {
            configurator.addAdditionalReservedWordMapping(entry.getKey(), entry.getValue());
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
