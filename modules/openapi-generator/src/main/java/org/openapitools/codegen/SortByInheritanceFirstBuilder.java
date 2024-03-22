package org.openapitools.codegen;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.Schema;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.utils.ModelUtils;

import java.util.*;
import java.util.stream.Collectors;

public class SortByInheritanceFirstBuilder {
    private final DefaultCodegen defaultCodegen;
    private final OpenAPI openAPI;
    private final Set<Schema> visitedSchemas = Collections.newSetFromMap(new IdentityHashMap<>());
//    private final Set<String> properties = new LinkedHashSet<>();
//    private final Map<String, Schema> allDefinitions;
    private List<String> properties = new ArrayList<>();

    SortByInheritanceFirstBuilder(DefaultCodegen defaultCodegen) {
        this.defaultCodegen = defaultCodegen;
        openAPI = defaultCodegen.openAPI;
//        this.allDefinitions = allDefinitions;
    }

    public void build(Schema schema) {
        if (!visitedSchemas.add(schema)) {
            // avoid infinite loop
            return;
        }

        if (ObjectUtils.isNotEmpty(schema.getAllOf())) {

            for (Object child : schema.getAllOf()) {
                Schema childSchema = getReferencedSchema((Schema) child);

                build(childSchema);
            }
        }
        if (ObjectUtils.isNotEmpty(schema.getOneOf())) {
            for (Object child : schema.getOneOf()) {
                Schema childSchema = getReferencedSchema((Schema) child);

                build(childSchema);
            }
        }
        if (ObjectUtils.isNotEmpty(schema.getAnyOf())) {
            for (Object child : schema.getAnyOf()) {
                Schema childSchema = getReferencedSchema((Schema) child);

                build(childSchema);
            }
        }
        if (schema.getProperties() != null) {
            Map map = defaultCodegen.unaliasPropertySchema(schema.getProperties());
            properties.addAll(map.keySet());
        }

    }


    private Schema getReferencedSchema(Schema schema) {
        while (schema != null && StringUtils.isNotEmpty(schema.get$ref())) {
            schema = ModelUtils.getReferencedSchema(openAPI, schema);
        }
        return schema;
    }

    public SortByInheritanceFirstBuilder buildAll(Schema schema) {
        build(schema);
        return this;
    }

    public void reorder(CodegenModel m) {
//        List<String> properties = getProperties(m);
        if (properties.isEmpty()) {
            return;
        }
        try {
            reorder( m.vars);
            reorder( m.allVars);
            reorder( m.requiredVars);
            reorder( m.optionalVars);
            reorder( m.readOnlyVars);
            reorder( m.readWriteVars);
            reorder( m.parentVars);
//            reorder(properties, m.parentRequiredVars);
            reorder( m.nonNullableVars);
        } catch (IllegalArgumentException e) {
            throw new IllegalArgumentException("unable to reorder " + m.schemaName, e);
        }
    }

    void reorder(List<CodegenProperty> vars) {
        if (vars.isEmpty()) {
            return;
        }
        List<CodegenProperty> copy = new ArrayList<>(vars);
        vars.clear();
        for (Iterator<String> it = properties.iterator(); it.hasNext(); ) {
            String name = it.next();
            // use latest match
            for (int i = copy.size() - 1; i >= 0; i--) {
                CodegenProperty property = copy.get(i);
                if (property.getBaseName().equals(name)) {
                    vars.add(property);
                    break;
                }
            }
        }
        if (vars.size() != copy.size()) {
            throw new IllegalArgumentException("No matching properties " + properties + " with vars " + copy.stream().map(CodegenProperty::getBaseName).collect(Collectors.joining(",")));
        }
    }

    public SortByInheritanceFirstBuilder validate(CodegenModel m) {
        if (true) return this;
//        List<String> properties = getProperties(m);
        List<String> notFound = m.allVars.stream()
                .map(CodegenProperty::getBaseName).filter(p -> !properties.contains(p))
                .collect(Collectors.toList());
        if (!notFound.isEmpty() || m.allVars.size() != properties.size()) {
            String allVars = m.allVars.stream().map(CodegenProperty::getBaseName).distinct().collect(Collectors.joining(",", "[", "]"));
            String found = properties.stream().collect(Collectors.joining(",", "[", "]"));
            throw new IllegalArgumentException("Expecting " + m.name + " to have all of " + found + ". Missing " + notFound +  " allvars:" + allVars + " computed:" + properties);
        }
        return this;
    }

//    private static List<String> getProperties(CodegenModel m) {
//        List<String> properties = (List<String>)m.vendorExtensions.get(CodegenConstants.SORT_MODEL_PROPERTIES_BY_INHERITANCE_FIRST);
//        if (properties == null) {
//            throw new IllegalArgumentException("Sorted properties not found for " + m.schemaName);
//        }
//        return properties;
//    }
}
