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

package org.openapitools.codegen.languages;

import io.swagger.v3.oas.models.media.Schema;
import lombok.Getter;
import lombok.Setter;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.model.OperationMap;
import org.openapitools.codegen.model.OperationsMap;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;

/**
 * <p>Mustache templates are located in {@code src/main/resources/php-nextgen/}.
 */
public class PhpNextgenClientCodegen extends AbstractPhpCodegen {
    @SuppressWarnings("hiding")
    private final Logger LOGGER = LoggerFactory.getLogger(PhpNextgenClientCodegen.class);

    public static final String SUPPORT_STREAMING = "supportStreaming";

    @Getter @Setter
    protected boolean supportStreaming = false;

    public PhpNextgenClientCodegen() {
        super();

        // override default src and test folders to comply PSD skeleton
        setSrcBasePath("src");
        setTestBasePath("tests");

        // mark as beta so far
        this.generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.BETA).build();

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON, WireFormatFeature.XML))
                .securityFeatures(EnumSet.noneOf(SecurityFeature.class))
                .excludeGlobalFeatures(
                        GlobalFeature.XMLStructureDefinitions,
                        GlobalFeature.Callbacks,
                        GlobalFeature.LinkObjects,
                        GlobalFeature.ParameterStyling
                )
        );

        // clear import mapping (from default generator) as php does not use it
        // at the moment
        importMapping.clear();

        setInvokerPackage("OpenAPI\\Client");
        setApiPackage(getInvokerPackage() + "\\" + apiDirName);
        setModelPackage(getInvokerPackage() + "\\" + modelDirName);
        setPackageName("OpenAPIClient-php");
        supportsInheritance = true;
        setOutputDir("generated-code" + File.separator + "php");
        modelTestTemplateFiles.put("model_test.mustache", ".php");
        embeddedTemplateDir = templateDir = "php-nextgen";

        // default HIDE_GENERATION_TIMESTAMP to true
        hideGenerationTimestamp = Boolean.TRUE;

        // provide primitives to mustache template
        List sortedLanguageSpecificPrimitives = new ArrayList(languageSpecificPrimitives);
        Collections.sort(sortedLanguageSpecificPrimitives);
        String primitives = "'" + StringUtils.join(sortedLanguageSpecificPrimitives, "', '") + "'";
        additionalProperties.put("primitives", primitives);

        cliOptions.add(new CliOption(CodegenConstants.HIDE_GENERATION_TIMESTAMP, CodegenConstants.ALLOW_UNICODE_IDENTIFIERS_DESC)
                .defaultValue(Boolean.TRUE.toString()));
        cliOptions.add(CliOption.newBoolean(SUPPORT_STREAMING, "Support streaming endpoint", this.supportStreaming));
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "php-nextgen";
    }

    @Override
    public String getHelp() {
        return "Generates a PHP client library (beta).";
    }

    @Override
    public void processOpts() {
        super.processOpts();

        convertPropertyToBooleanAndWriteBack(SUPPORT_STREAMING, this::setSupportStreaming);

        supportingFiles.add(new SupportingFile("ApiException.mustache", toSrcPath(invokerPackage, srcBasePath), "ApiException.php"));
        supportingFiles.add(new SupportingFile("Configuration.mustache", toSrcPath(invokerPackage, srcBasePath), "Configuration.php"));
        supportingFiles.add(new SupportingFile("FormDataProcessor.mustache", toSrcPath(invokerPackage, srcBasePath), "FormDataProcessor.php"));
        supportingFiles.add(new SupportingFile("ObjectSerializer.mustache", toSrcPath(invokerPackage, srcBasePath), "ObjectSerializer.php"));
        supportingFiles.add(new SupportingFile("ModelInterface.mustache", toSrcPath(modelPackage, srcBasePath), "ModelInterface.php"));
        supportingFiles.add(new SupportingFile("OneOfInterface.mustache", toSrcPath(modelPackage, srcBasePath), "OneOfInterface.php"));
        supportingFiles.add(new SupportingFile("AnyOfInterface.mustache", toSrcPath(modelPackage, srcBasePath), "AnyOfInterface.php"));
        supportingFiles.add(new SupportingFile("HeaderSelector.mustache", toSrcPath(invokerPackage, srcBasePath), "HeaderSelector.php"));
        supportingFiles.add(new SupportingFile("composer.mustache", "", "composer.json"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("phpunit.xml.mustache", "", "phpunit.xml.dist"));
        supportingFiles.add(new SupportingFile(".travis.yml", "", ".travis.yml"));
        supportingFiles.add(new SupportingFile(".php-cs-fixer.dist.php", "", ".php-cs-fixer.dist.php"));
        supportingFiles.add(new SupportingFile(".phplint.mustache", "", ".phplint.yml"));
        supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));

        if (this.supportStreaming) {
            typeMapping.put("file", "\\Psr\\\\Http\\\\Message\\\\StreamInterface");
        }
    }

    @Override
    public Map<String, ModelsMap> postProcessAllModels(Map<String, ModelsMap> objs) {
        final Map<String, ModelsMap> processed = super.postProcessAllModels(objs);

        Map<String, String> composedTypeHints = new HashMap<>();
        for (ModelsMap modelsMap : processed.values()) {
            for (ModelMap m : modelsMap.getModels()) {
                collectComposedTypeHint(m.getModel(), composedTypeHints);
            }
        }
        flattenComposedTypeHints(composedTypeHints);

        for (Map.Entry<String, ModelsMap> entry : processed.entrySet()) {
            entry.setValue(postProcessModelsMap(entry.getValue(), composedTypeHints));
        }

        return processed;
    }

    /**
     * If the given model is a oneOf or anyOf composition, record the PHP union type that should be
     * used wherever the model is referenced. A model that declares both contributes all members.
     */
    private void collectComposedTypeHint(CodegenModel model, Map<String, String> composedTypeHints) {
        Set<String> memberTypes = directComposedMemberTypes(model);
        if (memberTypes.isEmpty()) {
            return;
        }

        composedTypeHints.put("\\" + modelPackage + "\\" + model.classname, String.join("|", memberTypes));
    }

    /**
     * The immediate (non-recursive) oneOf/anyOf member types of a composed model, containers
     * collapsed to {@code array}. Empty when the model is not a composition.
     */
    private Set<String> directComposedMemberTypes(CodegenModel model) {
        Set<String> memberTypes = new LinkedHashSet<>();
        if (model == null || model.getComposedSchemas() == null) {
            return memberTypes;
        }

        CodegenComposedSchemas composed = model.getComposedSchemas();
        List<CodegenProperty> members = new ArrayList<>();
        if (composed.getOneOf() != null) {
            members.addAll(composed.getOneOf());
        }
        if (composed.getAnyOf() != null) {
            members.addAll(composed.getAnyOf());
        }
        for (CodegenProperty member : members) {
            memberTypes.add((member.isArray || member.isMap) ? "array" : member.dataType);
        }
        return memberTypes;
    }

    /**
     * Split a flattened union into doc-link entries. A class member (starts with {@code \}) gets a
     * {@code complexType} - its bare class name - for the {@code .md} link; a primitive gets none.
     */
    private List<Map<String, String>> composedLeafDocEntries(String union) {
        List<Map<String, String>> entries = new ArrayList<>();
        for (String leaf : union.split("\\|")) {
            Map<String, String> entry = new HashMap<>();
            entry.put("dataType", leaf);
            if (leaf.startsWith("\\")) {
                entry.put("complexType", leaf.substring(leaf.lastIndexOf('\\') + 1));
            }
            entries.add(entry);
        }
        return entries;
    }

    /**
     * Expand each composed type's union hint transitively: a member that is itself a composed type
     * is replaced by its own leaf members. The generated {@code ObjectSerializer} dispatches
     * nested composition down to the leaf instance, so a property typed with an intermediate
     * composed member would otherwise reject the leaf the deserializer actually returns.
     */
    private void flattenComposedTypeHints(Map<String, String> composedTypeHints) {
        Map<String, String> resolved = new HashMap<>();
        for (String composedType : composedTypeHints.keySet()) {
            Set<String> leaves = new LinkedHashSet<>();
            collectLeafTypes(composedType, composedTypeHints, new LinkedHashSet<>(), leaves);
            resolved.put(composedType, String.join("|", leaves));
        }
        composedTypeHints.putAll(resolved);
    }

    /**
     * Accumulate into {@code leaves} the non-composed member types reachable from {@code type}. A
     * member that is itself a composed type (a key in {@code composedTypeHints}) is expanded
     * recursively; {@code visiting} guards against cycles in self-referential schemas.
     */
    private void collectLeafTypes(String type, Map<String, String> composedTypeHints, Set<String> visiting, Set<String> leaves) {
        if (!composedTypeHints.containsKey(type)) {
            leaves.add(type);
            return;
        }
        if (!visiting.add(type)) {
            return;
        }
        for (String member : composedTypeHints.get(type).split("\\|")) {
            collectLeafTypes(member, composedTypeHints, visiting, leaves);
        }
        visiting.remove(type);
    }

    /**
     * PHP forbids the nullable shorthand ({@code ?T}) on union types, so a union must instead
     * gain an explicit {@code |null} member.
     */
    private static String makeNullable(String phpType) {
        return phpType.contains("|") ? phpType + "|null" : "?" + phpType;
    }

    /**
     * The base PHP type hint for a single element: a container collapses to {@code array} (PHP
     * cannot type-hint {@code Foo[]}), a composed (oneOf/anyOf) alias expands to the union of its members, and
     * everything else stays its {@code dataType}.
     */
    private String phpBaseType(String dataType, boolean isContainer, Map<String, String> composedTypeHints) {
        return isContainer ? "array" : composedTypeHints.getOrDefault(dataType, dataType);
    }

    /**
     * The PHP signature type hint: the {@link #phpBaseType base type}, made nullable when the
     * element is optional or nullable - except {@code mixed}, which already admits null.
     */
    private String phpSignatureType(String dataType, boolean isContainer, boolean nullable, Map<String, String> composedTypeHints) {
        String base = phpBaseType(dataType, isContainer, composedTypeHints);
        return (nullable && !base.equals("mixed")) ? makeNullable(base) : base;
    }

    /**
     * Wrap an expanded inner union back into container phpdoc notation: {@code (Apple|Banana)[]}
     * for arrays (parenthesised so {@code []} binds to the whole union, not just its last member)
     * and {@code array<string,Apple|Banana>} for maps. A {@code null} inner propagates, signalling
     * "no composed schema in this type".
     */
    private static String wrapContainerDoc(boolean isArray, String inner) {
        if (inner == null) {
            return null;
        }
        return isArray ? (inner.contains("|") ? "(" + inner + ")[]" : inner + "[]")
                : "array<string," + inner + ">";
    }

    /**
     * The phpdoc type with any reference to a composed (oneOf/anyOf) model expanded to the union of its members.
     * A composed model is only a deserialization dispatcher, so its members do not inherit from it
     * and {@code @param Fruit} would be a lie — {@code @param Apple|Banana} is the truth.
     * Returns {@code null} when no composed model is involved, so the caller can leave the original
     * {@code dataType} phpdoc untouched.
     */
    private String composedDocType(CodegenProperty prop, Map<String, String> composedTypeHints) {
        return docTypeOf(prop.isArray, prop.isMap, prop.items, prop.dataType, composedTypeHints);
    }

    /** @see #composedDocType(CodegenProperty, Map) */
    private String composedDocType(CodegenParameter param, Map<String, String> composedTypeHints) {
        return docTypeOf(param.isArray, param.isMap, param.items, param.dataType, composedTypeHints);
    }

    /** @see #composedDocType(CodegenProperty, Map) */
    private String composedDocType(CodegenResponse response, Map<String, String> composedTypeHints) {
        return docTypeOf(response.isArray, response.isMap, response.items, response.dataType, composedTypeHints);
    }

    /**
     * The shared core of the {@code composedDocType} overloads: expands a composed {@code dataType} to the
     * union of its members (recursing through array/map items so the expansion reaches nested composed schemas),
     * or returns {@code null} when no composed model is involved. See {@link #composedDocType(CodegenProperty, Map)}.
     */
    private String docTypeOf(boolean isArray, boolean isMap, CodegenProperty items, String dataType, Map<String, String> composedTypeHints) {
        if ((isArray || isMap) && items != null) {
            return wrapContainerDoc(isArray, composedDocType(items, composedTypeHints));
        }
        return composedTypeHints.get(dataType);
    }

    /**
     * The final phpdoc type, ready for the template to emit verbatim: the union-expanded type (or the
     * unchanged {@code dataType} when no composed model is involved), with a {@code |null} member appended
     * when the element is optional or nullable. phpdoc unions always spell out {@code |null}
     * rather than using the {@code ?T} shorthand.
     */
    private String phpDocType(CodegenProperty prop, Map<String, String> composedTypeHints) {
        return bakeDocType(composedDocType(prop, composedTypeHints), prop.dataType, prop.notRequiredOrIsNullable());
    }

    private String phpDocType(CodegenParameter param, Map<String, String> composedTypeHints) {
        return bakeDocType(composedDocType(param, composedTypeHints), param.dataType, param.notRequiredOrIsNullable());
    }

    /**
     * The shared core of the {@code phpDocType} overloads: uses {@code expandedType}, falling back to
     * {@code dataType} when it is {@code null}, and appends {@code |null} when {@code nullable}.
     * See {@link #phpDocType(CodegenProperty, Map)}.
     */
    private static String bakeDocType(String expandedType, String dataType, boolean nullable) {
        String docType = expandedType != null ? expandedType : dataType;
        return nullable ? docType + "|null" : docType;
    }

    /**
     * A composed model is an abstract dispatcher, so the default doc example ({@code new Mammal()})
     * instantiates a type that cannot be used. Rewrite the example to instantiate the first member
     * of the union instead ({@code new Whale()}). Handles a composed parameter directly as well as a
     * container whose items are composed.
     */
    private void useFirstComposedMemberInExample(CodegenParameter param, Map<String, String> composedTypeHints) {
        if (param.example == null) {
            return;
        }
        String alias = composedTypeHints.containsKey(param.dataType) ? param.dataType
                : (param.items != null && composedTypeHints.containsKey(param.items.dataType) ? param.items.dataType : null);
        if (alias == null) {
            return;
        }
        String firstMember = composedTypeHints.get(alias).split("\\|", 2)[0];
        if (firstMember.startsWith("\\")) { // a concrete class we can instantiate
            param.example = param.example.replace(alias, firstMember);
        }
    }

    private ModelsMap postProcessModelsMap(ModelsMap objs, Map<String, String> composedTypeHints) {
        for (ModelMap m : objs.getModels()) {
            CodegenModel model = m.getModel();

            // Surface a composed model's flattened leaf types so the doc page lists the concrete
            // types users actually work with, matching the generated method signatures.
            String composedKey = "\\" + modelPackage + "\\" + model.classname;
            if (composedTypeHints.containsKey(composedKey)) {
                model.vendorExtensions.putIfAbsent("x-php-composed-leaves",
                        composedLeafDocEntries(composedTypeHints.get(composedKey)));
            }

            for (CodegenProperty prop : model.vars) {
                prop.vendorExtensions.putIfAbsent("x-php-prop-type",
                        phpSignatureType(prop.dataType, prop.isArray || prop.isMap, prop.notRequiredOrIsNullable(), composedTypeHints));
                prop.vendorExtensions.putIfAbsent("x-php-prop-doc-type", phpDocType(prop, composedTypeHints));
            }
        }
        return objs;
    }

    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap objs, List<ModelMap> allModels) {
        objs = super.postProcessOperationsWithModels(objs, allModels);

        Map<String, String> composedTypeHints = new HashMap<>();
        for (ModelMap m : allModels) {
            collectComposedTypeHint(m.getModel(), composedTypeHints);
        }
        flattenComposedTypeHints(composedTypeHints);

        OperationMap operations = objs.getOperations();
        for (CodegenOperation operation : operations.getOperation()) {
            Set<String> phpReturnTypeOptions = new LinkedHashSet<>();
            Set<String> docReturnTypeOptions = new LinkedHashSet<>();
            boolean hasEmptyResponse = false;

            for (CodegenResponse response : operation.responses) {
                if (response.dataType != null) {
                    // The signature collapses a container to `array` (PHP cannot type-hint Foo[]);
                    // the phpdoc keeps the full notation, with any composed alias expanded to its union.
                    phpReturnTypeOptions.add(phpBaseType(response.dataType, response.isArray || response.isMap, composedTypeHints));
                    String responseDocType = composedDocType(response, composedTypeHints);
                    docReturnTypeOptions.add(responseDocType != null ? responseDocType : response.dataType);
                } else if (response.is2xx) {
                    // Only a body-less *success* response makes the method return null. A body-less
                    // error response throws an ApiException instead, so it must not make the return
                    // type nullable.
                    hasEmptyResponse = true;
                }
            }

            if (phpReturnTypeOptions.isEmpty()) {
                operation.vendorExtensions.putIfAbsent("x-php-return-type-is-void", true);
                operation.vendorExtensions.putIfAbsent("x-php-return-type", "void");
                operation.vendorExtensions.putIfAbsent("x-php-doc-return-type", "void");
            } else {
                String phpReturnType = String.join("|", phpReturnTypeOptions);
                String docReturnType = String.join("|", docReturnTypeOptions);
                if (hasEmptyResponse) {
                    phpReturnType = makeNullable(phpReturnType);
                    docReturnType = docReturnType + "|null";
                }

                operation.vendorExtensions.putIfAbsent("x-php-return-type-is-void", false);
                operation.vendorExtensions.putIfAbsent("x-php-return-type", phpReturnType);
                operation.vendorExtensions.putIfAbsent("x-php-doc-return-type", docReturnType);
            }

            for (CodegenParameter param : operation.allParams) {
                param.vendorExtensions.putIfAbsent("x-php-param-type",
                        phpSignatureType(param.dataType, param.isArray || param.isMap, param.notRequiredOrIsNullable(), composedTypeHints));
                param.vendorExtensions.putIfAbsent("x-php-param-doc-type", phpDocType(param, composedTypeHints));
                useFirstComposedMemberInExample(param, composedTypeHints);
            }
        }

        return objs;
    }

    @Override
    public String toDefaultValue(CodegenProperty codegenProperty, Schema schema) {

        if (codegenProperty.isArray) {
            schema = ModelUtils.getReferencedSchema(this.openAPI, schema);

            if (schema.getDefault() != null) { // array schema has default value
                return schema.getDefault().toString();
            } else if (schema.getItems().getDefault() != null) { // array item schema has default value
                return "[" + toDefaultValue(schema.getItems()) + "]";
            } else {
                return null;
            }
        }
        return super.toDefaultValue(codegenProperty, schema);
    }

    @Override
    public String toDefaultParameterValue(CodegenProperty codegenProperty, Schema<?> schema) {
        return toDefaultValue(codegenProperty, schema);
    }

    @Override
    public void setParameterExampleValue(CodegenParameter p) {
        if (p.isArray && p.items.defaultValue != null) {
            p.example = p.defaultValue;
        } else {
            super.setParameterExampleValue(p);
        }
    }
}
