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

package org.openapitools.codegen.config;

import org.apache.commons.lang3.tuple.Pair;
import org.openapitools.codegen.utils.OptionUtils;

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

    public static void applyGlobalPropertiesKvpList(List<String> globalProperties, CodegenConfigurator configurator) {
        for(String propString : globalProperties) {
            applyGlobalPropertiesKvp(propString, configurator);
        }
    }

    public static void applyGlobalPropertiesKvp(String globalProperties, CodegenConfigurator configurator) {
        final Map<String, String> map = createMapFromKeyValuePairs(globalProperties);
        for (Map.Entry<String, String> entry : map.entrySet()) {
            configurator.addGlobalProperty(entry.getKey(), entry.getValue());
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

    public static void applyServerVariablesKvpList(List<String> values, CodegenConfigurator configurator) {
        for(String value : values) {
            applyServerVariablesKvp(value, configurator);
        }
    }

    public static void applyServerVariablesKvp(String values, CodegenConfigurator configurator) {
        final Map<String, String> map = createMapFromKeyValuePairs(values);
        for (Map.Entry<String, String> entry : map.entrySet()) {
            configurator.addServerVariable(entry.getKey(), entry.getValue());
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
