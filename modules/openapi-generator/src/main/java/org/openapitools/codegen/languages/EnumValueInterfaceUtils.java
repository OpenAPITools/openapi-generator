package org.openapitools.codegen.languages;

import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.DefaultCodegen;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Language-agnostic utility for the {@code useEnumValueInterface} code-generation option.
 *
 * <p>When enabled, every generated enum (both top-level schema enums and inline property enums)
 * is made to implement a common {@code ValuedEnum<T>} interface that exposes the backing value,
 * allowing generic code to access enum values without reflection.</p>
 *
 * <p>Two entry points are provided, one for each generator lifecycle hook:
 * <ul>
 *   <li>{@link #setupInPreprocessOpenAPI} — called from {@code preprocessOpenAPI} to register
 *       the supporting file and the import mapping.</li>
 *   <li>{@link #injectInPostProcessModelsEnum} — called from {@code postProcessModelsEnum} to
 *       inject the interface into every enum model's implements vendor extension.</li>
 * </ul>
 *
 * <p>The only language-specific knobs are:</p>
 * <ul>
 *   <li>The name of the vendor extension that carries implemented interfaces
 *       ({@code "x-implements"} for Java, {@code "x-kotlin-implements"} for Kotlin).</li>
 *   <li>The output file name ({@code "ValuedEnum.java"} vs {@code "ValuedEnum.kt"}).</li>
 * </ul>
 *
 * <p>Used by both {@link SpringCodegen} and {@link KotlinSpringServerCodegen}.</p>
 */
public final class EnumValueInterfaceUtils {

    private EnumValueInterfaceUtils() {}

    /**
     * Registers the {@code ValuedEnum} supporting file and import mapping.
     *
     * <p>Must be called from {@code preprocessOpenAPI} when {@code useEnumValueInterface} is
     * enabled. Returns the simple class name derived from the (possibly custom) import mapping,
     * which the caller must store and pass to {@link #injectInPostProcessModelsEnum} later.</p>
     *
     * @param importMapping      the codegen's import-mapping map (mutated in place)
     * @param additionalProperties the codegen's additional-properties map (mutated in place)
     * @param supportingFiles    the codegen's supporting-files list (mutated in place)
     * @param sourceFolder       language source folder (e.g. {@code "src/main/java"})
     * @param configPackage      the config package where the interface is generated
     *                           (e.g. {@code "org.openapitools.configuration"})
     * @param mustacheTemplate   template name (e.g. {@code "enumValueInterface.mustache"})
     * @param outputFileName     generated file name (e.g. {@code "ValuedEnum.java"})
     * @return the simple class name of {@code ValuedEnum} (accounts for custom import mappings)
     */
    public static String setupInPreprocessOpenAPI(
            Map<String, String> importMapping,
            Map<String, Object> additionalProperties,
            List<SupportingFile> supportingFiles,
            String sourceFolder,
            String configPackage,
            String mustacheTemplate,
            String outputFileName) {

        boolean customMapping = importMapping.containsKey("ValuedEnum");
        importMapping.putIfAbsent("ValuedEnum", configPackage + ".ValuedEnum");
        if (!customMapping) {
            supportingFiles.add(new SupportingFile(mustacheTemplate,
                    (sourceFolder + File.separator + configPackage).replace(".", File.separator),
                    outputFileName));
        }
        String fqn = importMapping.get("ValuedEnum");
        String className = fqn.substring(fqn.lastIndexOf('.') + 1);
        additionalProperties.put("useEnumValueInterface", true);
        return className;
    }

    /**
     * Injects {@code ValuedEnum<T>} into the implements vendor extension of every enum in the
     * given model batch.
     *
     * <p>Must be called from {@code postProcessModelsEnum} when {@code useEnumValueInterface} is
     * enabled.  Handles both top-level enum schemas and inline enum properties.</p>
     *
     * @param objs                   the model batch being post-processed
     * @param valuedEnumClassName    simple class name (e.g. {@code "ValuedEnum"})
     * @param valuedEnumFqn          fully-qualified name used for the import statement
     * @param xImplementsExtensionKey vendor-extension key that carries the implements list
     *                               ({@code "x-implements"} for Java,
     *                               {@code "x-kotlin-implements"} for Kotlin)
     */
    public static void injectInPostProcessModelsEnum(
            ModelsMap objs,
            String valuedEnumClassName,
            String valuedEnumFqn,
            String xImplementsExtensionKey) {

        List<Map<String, String>> imports = objs.getImports();
        for (ModelMap mo : objs.getModels()) {
            CodegenModel cm = mo.getModel();
            boolean needsImport = false;

            if (cm.isEnum && cm.allowableValues != null) {
                List<String> xImpl = new ArrayList<>(
                        DefaultCodegen.getObjectAsStringList(cm.getVendorExtensions().get(xImplementsExtensionKey)));
                xImpl.add(valuedEnumClassName + "<" + cm.dataType + ">");
                cm.getVendorExtensions().put(xImplementsExtensionKey, xImpl);
                needsImport = true;
            }

            for (CodegenProperty var : cm.vars) {
                if (var.isEnum && !var.isContainer) {
                    List<String> xVarImpl = new ArrayList<>(
                            DefaultCodegen.getObjectAsStringList(var.getVendorExtensions().get(xImplementsExtensionKey)));
                    xVarImpl.add(valuedEnumClassName + "<" + var.dataType + ">");
                    var.getVendorExtensions().put(xImplementsExtensionKey, xVarImpl);
                    needsImport = true;
                }
            }

            if (needsImport) {
                cm.imports.add(valuedEnumFqn);
                Map<String, String> importItem = new HashMap<>();
                importItem.put("import", valuedEnumFqn);
                imports.add(importItem);
            }
        }
    }
}
