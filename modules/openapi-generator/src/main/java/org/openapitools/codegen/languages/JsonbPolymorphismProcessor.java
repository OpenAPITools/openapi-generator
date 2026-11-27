/*
 * Copyright 2026 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

import org.openapitools.codegen.CodegenDiscriminator;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.DefaultCodegen;
import org.openapitools.codegen.VendorExtension;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;

import static org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen.isAbstract;
import static org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen.isOneOfInterface;

/**
 * Prepares the vendor extensions of the {@code jsonbDiscriminatorDeserializers} option: a JSON-B deserializer for every model whose
 * discriminator maps subtypes (see {@code jsonbPolymorphicDeserializer.mustache}), and the discriminator values that
 * concrete models are created with.
 * <p>
 * JSON-B 3.0 {@code @JsonbTypeInfo} is not used: it requires the type key not to be a property of the class, but the
 * generated models declare the discriminator as a property (Yasson fails to serialize such a model, Johnzon rejects it).
 * <p>
 * The dispatch table of a model is the part of its discriminator mapping that targets the model itself or a subtype;
 * with {@code legacyDiscriminatorBehavior=false} a child carries a copy of its parent's mapping, which also lists
 * siblings and ancestors. A concrete model that no value maps to itself is also resolved under its Jackson type name.
 * Each value is resolved by the deserializer of its target if it has one, otherwise by binding the target directly;
 * the model itself is bound through a transfer object, which avoids recursing into its own deserializer.
 */
final class JsonbPolymorphismProcessor {

    private final Logger LOGGER = LoggerFactory.getLogger(JsonbPolymorphismProcessor.class);

    private final DefaultCodegen codegen;
    private final List<CodegenModel> allModels;
    // keyed by classname: CodegenModel#hashCode is deep and recurses through parent models
    private final Map<String, CodegenModel> byClassname = new HashMap<>();
    private final Map<String, DispatchTable> dispatchTables = new LinkedHashMap<>();

    JsonbPolymorphismProcessor(DefaultCodegen codegen, List<CodegenModel> allModels) {
        this.codegen = codegen;
        this.allModels = allModels;
        allModels.forEach(model -> byClassname.put(model.classname, model));
    }

    void apply() {
        for (CodegenModel model : allModels) {
            DispatchTable table = dispatchTableOf(model);
            if (table != null) {
                dispatchTables.put(model.classname, table);
            }
        }
        // the helper classes are nested in the models and shadow equally named types in them and in their subclasses
        Set<String> usedTypeNames = usedTypeNames();
        String deserializerName = uniqueTypeName("JsonbPolymorphicDeserializer", usedTypeNames);
        usedTypeNames.add(deserializerName);
        String dataName = uniqueTypeName("JsonbData", usedTypeNames);
        usedTypeNames.add(dataName);
        String containerDeserializerName = uniqueTypeName("JsonbPolymorphicContainerDeserializer", usedTypeNames);
        dispatchTables.forEach((classname, table) -> applyDeserializer(byClassname.get(classname), table, deserializerName, dataName));
        applyContainerDeserializers(containerDeserializerName);
        applyDiscriminatorDefaults();
    }

    /**
     * Gives every list, set or map property (also a nested one) whose elements are of a model with a deserializer the
     * container deserializer of that model ({@code jsonbPolymorphicContainerDeserializer.mustache}): Yasson up to 3.0.4
     * does not apply the class-level deserializer to the elements of some containers (fixed in 3.0.5).
     */
    private void applyContainerDeserializers(String name) {
        for (CodegenModel model : allModels) {
            // the lists the templates annotate, which hold separate instances of a property: vars (setters), requiredVars
            // (creator parameters, also inherited ones) and allVars (fields of the transfer object JsonbData)
            for (List<CodegenProperty> vars : Arrays.asList(model.vars, model.requiredVars, model.allVars)) {
                for (CodegenProperty var : vars) {
                    CodegenProperty element = var;
                    while (element.items != null) {
                        element = element.items;
                    }
                    if (var.isContainer && element.complexType != null && dispatchTables.containsKey(element.complexType)) {
                        byClassname.get(element.complexType).getVendorExtensions().put("x-jsonb-container-deserializer-name", name);
                        var.getVendorExtensions().put("x-jsonb-container-deserializer", qualifiedModelName(element.complexType) + "." + name);
                    }
                }
            }
        }
    }

    /**
     * The dispatch table of the model, or {@code null} if it does not map a subtype. An intermediate class without a
     * discriminator of its own ({@code legacyDiscriminatorBehavior=true}) dispatches by that of its nearest ancestor, like
     * Jackson does through the inherited {@code @JsonTypeInfo}.
     */
    private DispatchTable dispatchTableOf(CodegenModel model) {
        CodegenDiscriminator discriminator = model.getDiscriminator();
        for (CodegenModel ancestor = model.parentModel; discriminator == null && ancestor != null; ancestor = ancestor.parentModel) {
            discriminator = ancestor.getDiscriminator();
        }
        if (model.isEnum || discriminator == null || discriminator.getMappedModels() == null) {
            return null;
        }
        List<CodegenDiscriminator.MappedModel> mappings = discriminator.getMappedModels().stream()
                .filter(mappedModel -> {
                    CodegenModel target = byClassname.get(mappedModel.getModelName());
                    return target != null && isAssignableTo(target, model);
                })
                .collect(Collectors.toList());
        if (mappings.stream().allMatch(mappedModel -> mappedModel.getModelName().equals(model.classname))) {
            return null;
        }
        boolean mapsItself = mappings.stream().anyMatch(mappedModel -> mappedModel.getModelName().equals(model.classname));
        if (mapsItself && model.getAdditionalProperties() != null) {
            LOGGER.warn("No JSON-B deserializer is generated for model '{}': it declares additionalProperties and maps a discriminator "
                    + "value to itself, which '{}' cannot bind.", model.name, JavaJAXRSSpecServerCodegen.JSONB_DISCRIMINATOR_DESERIALIZERS);
            return null;
        }
        CodegenDiscriminator.MappedModel implicitSelf = null;
        if (!mapsItself && model.getAdditionalProperties() == null && !isAbstract(model) && !isOneOfInterface(model)) {
            implicitSelf = implicitSelfMapping(model, mappings);
            if (implicitSelf != null) {
                mappings.add(implicitSelf);
            }
        }
        return new DispatchTable(discriminator.getPropertyBaseName(), mappings, implicitSelf);
    }

    /**
     * The mapping of a concrete model to itself under its Jackson type name, or {@code null} if that name maps a subtype.
     */
    private CodegenDiscriminator.MappedModel implicitSelfMapping(CodegenModel model, List<CodegenDiscriminator.MappedModel> mappings) {
        Object discriminatorValue = model.getVendorExtensions().get(VendorExtension.X_DISCRIMINATOR_VALUE.getName());
        String typeName = discriminatorValue != null ? String.valueOf(discriminatorValue) : model.name;
        if (mappings.stream().anyMatch(mappedModel -> mappedModel.getMappingName().equals(typeName))) {
            LOGGER.warn("Model '{}' is not resolved as itself by its JSON-B deserializer: its type name '{}' is mapped to another model.",
                    model.name, typeName);
            return null;
        }
        return new CodegenDiscriminator.MappedModel(typeName, model.classname, model.schemaName, false);
    }

    private void applyDeserializer(CodegenModel model, DispatchTable table, String deserializerName, String dataName) {
        List<Map<String, Object>> subtypes = new ArrayList<>();
        boolean mapsItself = false;
        for (CodegenDiscriminator.MappedModel mappedModel : table.mappings) {
            String targetClassname = mappedModel.getModelName();
            boolean self = targetClassname.equals(model.classname);
            DispatchTable targetTable = self ? null : dispatchTables.get(targetClassname);
            // a target that resolves its subclasses by another property is itself if that property has no value
            boolean selfIfMissing = targetTable != null && !table.property.equals(targetTable.property) && targetTable.mapsItself(targetClassname);
            if (selfIfMissing) {
                byClassname.get(targetClassname).getVendorExtensions().put("x-jsonb-self-if-missing", true);
            }
            mapsItself |= self;
            Map<String, Object> subtype = new HashMap<>();
            subtype.put("jsonbValue", codegen.escapeText(mappedModel.getMappingName()));
            subtype.put("jsonbType", qualifiedModelName(targetClassname));
            subtype.put("jsonbSelf", self);
            subtype.put("jsonbDelegate", targetTable != null);
            subtype.put("jsonbSelfIfMissing", selfIfMissing);
            subtype.put("jsonbBind", !self && targetTable == null);
            subtypes.add(subtype);
        }
        Map<String, Object> vendorExtensions = model.getVendorExtensions();
        vendorExtensions.put("x-jsonb-polymorphic", true);
        vendorExtensions.put("x-jsonb-deserializer-name", deserializerName);
        vendorExtensions.put("x-jsonb-data-name", dataName);
        vendorExtensions.put("x-jsonb-discriminator-property", codegen.escapeText(table.property));
        vendorExtensions.put("x-jsonb-self", mapsItself);
        vendorExtensions.put("x-jsonb-subtypes", subtypes);
    }

    /**
     * Sets the discriminator values of every concrete model. The constructor and the builder of a concrete ancestor run
     * first, so a value that an ancestor sets and the model does not (for example an ambiguous one) is reset to null.
     */
    private void applyDiscriminatorDefaults() {
        Map<String, List<Map<String, Object>>> ownDefaults = new HashMap<>();
        for (CodegenModel model : allModels) {
            if (!model.isEnum && !isAbstract(model) && !isOneOfInterface(model)) {
                ownDefaults.put(model.classname, discriminatorDefaults(model));
            }
        }
        ownDefaults.forEach((classname, own) -> {
            CodegenModel model = byClassname.get(classname);
            List<Map<String, Object>> defaults = new ArrayList<>(own);
            Set<String> properties = own.stream().map(entry -> (String) entry.get("jsonbDefaultProperty")).collect(Collectors.toSet());
            for (CodegenModel ancestor = model.parentModel; ancestor != null; ancestor = ancestor.parentModel) {
                for (Map<String, Object> inherited : ownDefaults.getOrDefault(ancestor.classname, Collections.emptyList())) {
                    if (properties.add((String) inherited.get("jsonbDefaultProperty"))) {
                        Map<String, Object> reset = new HashMap<>(inherited);
                        reset.put("jsonbDefaultValue", "null");
                        defaults.add(reset);
                    }
                }
            }
            if (!defaults.isEmpty()) {
                model.getVendorExtensions().put("x-jsonb-discriminator-defaults", defaults);
            }
        });
    }

    /**
     * The discriminator values of a concrete model, one per discriminator property of the model and its supertypes, in
     * this order of preference: the value an explicit mapping assigns to the model; the value that maps to the nearest
     * ancestor that resolves its subclasses by another property (for example {@code kind=CAR} for a subclass of
     * {@code Car}, which is resolved by {@code carType}); the value an implicit mapping assigns to the model. A property
     * whose preferred values are ambiguous gets no default.
     */
    private List<Map<String, Object>> discriminatorDefaults(CodegenModel model) {
        Map<String, List<CodegenDiscriminator.MappedModel>> mappingsByProperty = new LinkedHashMap<>();
        for (CodegenModel supertype : allModels) {
            CodegenDiscriminator discriminator = supertype.getDiscriminator();
            if (discriminator != null && discriminator.getMappedModels() != null && isAssignableTo(model, supertype)) {
                mappingsByProperty.computeIfAbsent(discriminator.getPropertyBaseName(), property -> new ArrayList<>())
                        .addAll(discriminator.getMappedModels());
            }
        }
        DispatchTable ownTable = dispatchTables.get(model.classname);
        if (ownTable != null && ownTable.implicitSelf != null) {
            mappingsByProperty.computeIfAbsent(ownTable.property, property -> new ArrayList<>()).add(ownTable.implicitSelf);
        }

        List<Map<String, Object>> defaults = new ArrayList<>();
        mappingsByProperty.forEach((property, mappings) -> {
            Set<String> values = mappingNames(mappings, model.classname, true);
            for (CodegenModel ancestor = model.parentModel; values.isEmpty() && ancestor != null; ancestor = ancestor.parentModel) {
                DispatchTable ancestorTable = dispatchTables.get(ancestor.classname);
                if (ancestorTable != null && !property.equals(ancestorTable.property)) {
                    values = mappingNames(mappings, ancestor.classname, true);
                    if (values.isEmpty()) {
                        values = mappingNames(mappings, ancestor.classname, false);
                    }
                }
            }
            if (values.isEmpty()) {
                values = mappingNames(mappings, model.classname, false);
            }
            if (values.isEmpty()) {
                return;
            }
            if (values.size() > 1) {
                LOGGER.warn("Model '{}' is created without a value of its discriminator property '{}': the discriminator mappings assign "
                        + "it several values {}.", model.name, property, values);
                return;
            }
            String value = values.iterator().next();
            CodegenProperty var = model.allVars.stream().filter(candidate -> property.equals(candidate.baseName)).findFirst().orElse(null);
            String expression = var == null ? null : discriminatorValueExpression(model, var, value);
            if (expression == null) {
                LOGGER.warn("Model '{}' is created without a value of its discriminator property '{}': '{}' cannot be assigned to it.",
                        model.name, property, value);
                return;
            }
            Map<String, Object> discriminatorDefault = new HashMap<>();
            discriminatorDefault.put("jsonbDefaultProperty", property);
            discriminatorDefault.put("jsonbDefaultSetter", var.setter);
            discriminatorDefault.put("jsonbDefaultBuilderMethod", var.name);
            discriminatorDefault.put("jsonbDefaultValue", expression);
            defaults.add(discriminatorDefault);
        });
        return defaults;
    }

    private static Set<String> mappingNames(List<CodegenDiscriminator.MappedModel> mappings, String classname, boolean explicit) {
        return mappings.stream()
                .filter(mappedModel -> mappedModel.getModelName().equals(classname) && mappedModel.isExplicitMapping() == explicit)
                .map(CodegenDiscriminator.MappedModel::getMappingName)
                .collect(Collectors.toCollection(TreeSet::new));
    }

    /**
     * A string literal or the fully qualified constant of an inline or referenced enum, or {@code null} if the value is no
     * constant of the enum or the Java type is neither {@code String} nor an enum (a string of format uri is a {@code URI}).
     */
    private String discriminatorValueExpression(CodegenModel model, CodegenProperty property, String value) {
        if (property.isEnum) {
            // an inline enum is nested in the model that declares the property
            CodegenModel declaringModel = model;
            for (CodegenModel ancestor = model; ancestor != null; ancestor = ancestor.parentModel) {
                if (ancestor.vars.stream().anyMatch(var -> property.baseName.equals(var.baseName))) {
                    declaringModel = ancestor;
                    break;
                }
            }
            String constant = enumConstant(property.allowableValues, value, property.dataType);
            return constant == null ? null : qualifiedModelName(declaringModel.classname) + "." + property.datatypeWithEnum + "." + constant;
        }
        if (property.isEnumRef) {
            CodegenModel enumModel = byClassname.get(property.dataType);
            String constant = enumModel == null ? null : enumConstant(enumModel.allowableValues, value, enumModel.dataType);
            return constant == null ? null : qualifiedModelName(enumModel.classname) + "." + constant;
        }
        if ("String".equals(property.dataType) || "java.lang.String".equals(property.dataType)) {
            return "\"" + codegen.escapeText(value) + "\"";
        }
        return null;
    }

    @SuppressWarnings("unchecked")
    private String enumConstant(Map<String, Object> allowableValues, String value, String dataType) {
        Object enumVars = allowableValues == null ? null : allowableValues.get("enumVars");
        if (!(enumVars instanceof List)) {
            return null;
        }
        String literal = codegen.toEnumValue(value, dataType);
        for (Map<String, Object> enumVar : (List<Map<String, Object>>) enumVars) {
            if (literal.equals(String.valueOf(enumVar.get("value")))) {
                return String.valueOf(enumVar.get("name"));
            }
        }
        return null;
    }

    private String qualifiedModelName(String classname) {
        return codegen.modelPackage() + "." + classname;
    }

    /**
     * The simple type names that the models use: their class names, imports and the types of their properties.
     */
    private Set<String> usedTypeNames() {
        Set<String> names = new HashSet<>();
        for (CodegenModel model : allModels) {
            names.add(model.classname);
            model.imports.forEach(name -> names.add(name.substring(name.lastIndexOf('.') + 1)));
            for (List<CodegenProperty> vars : Arrays.asList(model.vars, model.allVars)) {
                for (CodegenProperty var : vars) {
                    for (String type : Arrays.asList(var.datatypeWithEnum, var.dataType, var.baseType, var.complexType)) {
                        if (type != null) {
                            names.addAll(Arrays.asList(type.split("[^\\p{javaJavaIdentifierPart}]+")));
                        }
                    }
                }
            }
        }
        return names;
    }

    private static String uniqueTypeName(String name, Set<String> usedTypeNames) {
        String unique = name;
        for (int i = 2; usedTypeNames.contains(unique); i++) {
            unique = name + i;
        }
        return unique;
    }

    /**
     * Whether instances of {@code target} are instances of {@code type}: the model itself, a subclass, or a class or
     * interface that implements the oneOf interface {@code type}, directly or through an ancestor or sub-interface.
     */
    private boolean isAssignableTo(CodegenModel target, CodegenModel type) {
        return isAssignableTo(target, type, new HashSet<>());
    }

    @SuppressWarnings("unchecked")
    private boolean isAssignableTo(CodegenModel target, CodegenModel type, Set<String> visited) {
        for (CodegenModel current = target; current != null; current = current.parentModel) {
            if (!visited.add(current.classname)) {
                return false;
            }
            if (current.classname.equals(type.classname)) {
                return true;
            }
            Object implemented = current.getVendorExtensions().get("x-implements");
            if (implemented instanceof List) {
                for (Object name : (List<Object>) implemented) {
                    CodegenModel implementedModel = byClassname.get(String.valueOf(name));
                    if (implementedModel != null && isAssignableTo(implementedModel, type, visited)) {
                        return true;
                    }
                }
            }
        }
        return false;
    }

    private static final class DispatchTable {
        private final String property;
        private final List<CodegenDiscriminator.MappedModel> mappings;
        /** the mapping of the model to itself under its type name, if no value maps it */
        private final CodegenDiscriminator.MappedModel implicitSelf;

        private DispatchTable(String property, List<CodegenDiscriminator.MappedModel> mappings, CodegenDiscriminator.MappedModel implicitSelf) {
            this.property = property;
            this.mappings = mappings;
            this.implicitSelf = implicitSelf;
        }

        private boolean mapsItself(String classname) {
            return mappings.stream().anyMatch(mappedModel -> mappedModel.getModelName().equals(classname));
        }
    }
}
