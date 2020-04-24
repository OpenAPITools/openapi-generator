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

import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.security.SecurityScheme;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.CodegenSecurity;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.utils.ModelUtils;
import org.openapitools.codegen.utils.ProcessUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;

import static org.openapitools.codegen.utils.StringUtils.camelize;

public class GoClientExperimentalCodegen extends GoClientCodegen {

    private static final Logger LOGGER = LoggerFactory.getLogger(GoClientExperimentalCodegen.class);

    public GoClientExperimentalCodegen() {
        super();
        outputFolder = "generated-code/go-experimental";
        embeddedTemplateDir = templateDir = "go-experimental";

        usesOptionals = false;
        useOneOfInterfaces = true;

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata).stability(Stability.EXPERIMENTAL).build();
    }

    /**
     * Configures a friendly name for the generator. This will be used by the
     * generator to select the library with the -g flag.
     *
     * @return the friendly name for the generator
     */
    @Override
    public String getName() {
        return "go-experimental";
    }

    @Override
    public String toGetter(String name) {
        return "Get" + getterAndSetterCapitalize(name);
    }

    /**
     * Returns human-friendly help for the generator. Provide the consumer with help
     * tips, parameters here
     *
     * @return A string value for the help message
     */
    @Override
    public String getHelp() {
        return "Generates a Go client library (experimental and may subject to breaking changes without further notice).";
    }

    @Override
    public void processOpts() {
        this.setLegacyDiscriminatorBehavior(false);
        super.processOpts();
        supportingFiles.add(new SupportingFile("utils.mustache", "", "utils.go"));

        // Generate the 'signing.py' module, but only if the 'HTTP signature' security scheme is specified in the OAS.
        Map<String, SecurityScheme> securitySchemeMap = openAPI != null ?
           (openAPI.getComponents() != null ? openAPI.getComponents().getSecuritySchemes() : null) : null;
        List<CodegenSecurity> authMethods = fromSecurity(securitySchemeMap);
        if (ProcessUtils.hasHttpSignatureMethods(authMethods)) {
            supportingFiles.add(new SupportingFile("signing.mustache", "", "signing.go"));
        }
    }

    @Override
    public String toModelName(String name) {
        // underscoring would also lowercase the whole name, thus losing acronyms which are in capitals
        return camelize(toModel(name, false));
    }

    public String escapeReservedWord(String name) {
        if (this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        return name + '_';
    }

    @Override
    public String toEnumDefaultValue(String value, String datatype) {
        String prefix = "";
        if (enumClassPrefix) {
            prefix = datatype.toUpperCase(Locale.ROOT) + "_";
        }
        return prefix + value;
    }

    @Override
    public void updateCodegenPropertyEnum(CodegenProperty var) {
        // make sure the inline enums have plain defaults (e.g. string, int, float)
        String enumDefault = null;
        if (var.isEnum && var.defaultValue != null) {
            enumDefault = var.defaultValue;
        }
        super.updateCodegenPropertyEnum(var);
        if (var.isEnum && enumDefault != null) {
            var.defaultValue = enumDefault;
        }
    }

    @Override
    public String toDefaultValue(Schema p) {
        p = ModelUtils.getReferencedSchema(this.openAPI, p);
        if (ModelUtils.isStringSchema(p)) {
            if (p.getDefault() != null) {
                return "\"" + escapeText((String) p.getDefault()) + "\"";
            }
            return null;
        }

        return super.toDefaultValue(p);
    }

    @Override
    public CodegenProperty fromProperty(String name, Schema p) {
        CodegenProperty prop = super.fromProperty(name, p);
        String cc = camelize(prop.name, true);
        if (isReservedWord(cc)) {
            cc = escapeReservedWord(cc);
        }
        prop.nameInCamelCase = cc;
        return prop;
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        // The superclass determines the list of required golang imports. The actual list of imports
        // depends on which types are used, some of which are changed in the code below (but then preserved
        // and used through x-go-base-type in templates). So super.postProcessModels
        // must be invoked at the beginning of this method.
        objs = super.postProcessModels(objs);

        List<Map<String, Object>> models = (List<Map<String, Object>>) objs.get("models");
        for (Map<String, Object> m : models) {
            Object v = m.get("model");
            if (v instanceof CodegenModel) {
                CodegenModel model = (CodegenModel) v;
                if (model.isEnum) {
                    continue;
                }

                for (CodegenProperty param : model.vars) {
                    param.vendorExtensions.put("x-go-base-type", param.dataType);
                    if (!param.isNullable || param.isMapContainer || param.isListContainer ||
                            param.isFreeFormObject || param.isAnyType) {
                        continue;
                    }
                    if (param.isDateTime) {
                        // Note this could have been done by adding the following line in processOpts(),
                        // however, we only want to represent the DateTime object as NullableTime if
                        // it's marked as nullable in the spec.
                        //    typeMapping.put("DateTime", "NullableTime");
                        param.dataType = "NullableTime";
                    } else {
                        param.dataType = "Nullable" + Character.toUpperCase(param.dataType.charAt(0))
                            + param.dataType.substring(1);
                    }
                }
            }
        }
        return objs;
    }

    @Override
    public void addImportsToOneOfInterface(List<Map<String, String>> imports) {
        for (String i : Arrays.asList("fmt")) {
            Map<String, String> oneImport = new HashMap<String, String>() {{
                put("import", i);
            }};
            if (!imports.contains(oneImport)) {
                imports.add(oneImport);
            }
        }
    }
}
