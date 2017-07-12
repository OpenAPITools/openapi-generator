package io.swagger.codegen.languages;

import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenProperty;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.models.properties.Property;

abstract public class AbstractCppCodegen extends DefaultCodegen implements CodegenConfig {

    @Override
    public String toVarName(String name) {
        if (typeMapping.keySet().contains(name) || typeMapping.values().contains(name)
                || importMapping.values().contains(name) || defaultIncludes.contains(name)
                || languageSpecificPrimitives.contains(name)) {
            return sanitizeName(name);
        }

        if (name.length() > 1) {
            return sanitizeName(Character.toUpperCase(name.charAt(0)) + name.substring(1));
        }

        return sanitizeName(name);
    }

    @Override
    public String toParamName(String name) {
        return sanitizeName(super.toParamName(name));
    }

    @Override
    public CodegenProperty fromProperty(String name, Property p) {
        CodegenProperty property = super.fromProperty(name, p);
        String nameInCamelCase = property.nameInCamelCase;
        if (nameInCamelCase.length() > 1) {
            nameInCamelCase = sanitizeName(Character.toLowerCase(nameInCamelCase.charAt(0)) + nameInCamelCase.substring(1));
        } else {
            nameInCamelCase = sanitizeName(nameInCamelCase);
        }
        property.nameInCamelCase = nameInCamelCase;
        return property;
    }
}
