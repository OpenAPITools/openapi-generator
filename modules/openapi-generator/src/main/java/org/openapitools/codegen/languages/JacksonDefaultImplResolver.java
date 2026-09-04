/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.VendorExtension;

import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.function.Consumer;
import java.util.function.Function;

/**
 * Shared resolution of the Jackson {@code defaultImpl} for oneOf interfaces.
 *
 * <p>The default implementation can be provided either per-schema via the
 * {@code x-jackson-default-impl} vendor extension, or via the {@code typeInfoDefaultImpls}
 * generator option (keyed by schema name). The config option takes precedence over the
 * vendor extension.
 *
 * <p>This logic is shared by {@link AbstractJavaCodegen}, {@link KotlinClientCodegen} and
 * {@link KotlinSpringServerCodegen}. The latter two are siblings under
 * {@code AbstractKotlinCodegen}, so the copy cannot be shared through inheritance; keeping it in
 * one place avoids the three implementations silently drifting apart.
 */
final class JacksonDefaultImplResolver {

    static final String RESOLVED_DEFAULT_IMPL = "x-jackson-resolved-default-impl";

    private JacksonDefaultImplResolver() {
    }

    /**
     * Resolve the {@code defaultImpl} class reference for the given model, if any.
     *
     * @param typeInfoDefaultImpls the {@code typeInfoDefaultImpls} config map (schema name to class)
     * @param cm                   the model being processed
     * @param toModelName          the generator's schema-name to model-name mapping
     * @param knownModelNames      the set of generated model names in this spec
     * @param warn                 sink for warning messages
     * @return the class reference to emit as {@code defaultImpl} (a generated model name, an external
     * simple class name, or {@code null} when nothing is configured)
     */
    static String resolve(Map<String, String> typeInfoDefaultImpls,
                          CodegenModel cm,
                          Function<String, String> toModelName,
                          Set<String> knownModelNames,
                          Consumer<String> warn) {
        Object rawAnnotationExt = cm.vendorExtensions.get(VendorExtension.X_JACKSON_DEFAULT_IMPL.getName());
        String schemaAnnotation = rawAnnotationExt instanceof String ? (String) rawAnnotationExt : null;
        String configValue = typeInfoDefaultImpls == null ? null : typeInfoDefaultImpls.get(cm.schemaName);

        String rawValue;
        if (configValue != null && !configValue.isBlank()) {
            if (schemaAnnotation != null && !schemaAnnotation.isBlank()) {
                warn.accept(String.format(Locale.ROOT, "typeInfoDefaultImpls overrides x-jackson-default-impl on schema '%s': '%s' \u2192 '%s'",
                        cm.schemaName, schemaAnnotation, configValue));
            }
            rawValue = configValue;
        } else if (schemaAnnotation != null && !schemaAnnotation.isBlank()) {
            rawValue = schemaAnnotation;
        } else {
            return null;
        }

        String resolvedModelName = toModelName.apply(rawValue);
        if (resolvedModelName != null && knownModelNames.contains(resolvedModelName)) {
            // A generated model in this spec: its import is already contributed by the oneOf
            // interface (mappedModels / interfaceModels), so only the class name is needed.
            return resolvedModelName;
        }

        // Not a generated model: this is a valid external or catch-all class. Preserve the name
        // verbatim instead of running it through toModelName (which would mangle a qualified name),
        // and register the import when it is fully qualified so the reference resolves.
        warn.accept(String.format(Locale.ROOT, "x-jackson-default-impl / typeInfoDefaultImpls on schema '%s' refers to '%s' which is not a known model in this spec. "
                + "This is valid for external or catch-all classes, but may indicate a typo.", cm.schemaName, rawValue));
        int lastDot = rawValue.lastIndexOf('.');
        if (lastDot > 0 && lastDot < rawValue.length() - 1) {
            cm.imports.add(rawValue);
            return rawValue.substring(lastDot + 1);
        }
        return rawValue;
    }
}
