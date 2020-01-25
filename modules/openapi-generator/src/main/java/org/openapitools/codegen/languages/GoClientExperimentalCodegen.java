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
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;
import java.util.Map;

public class GoClientExperimentalCodegen extends GoClientCodegen {

    private static final Logger LOGGER = LoggerFactory.getLogger(GoClientExperimentalCodegen.class);

    public GoClientExperimentalCodegen() {
        super();
        outputFolder = "generated-code/go-experimental";
        embeddedTemplateDir = templateDir = "go-experimental";

        usesOptionals = false;

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
        super.processOpts();
        supportingFiles.add(new SupportingFile("utils.mustache", "", "utils.go"));
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {

        List<Map<String, Object>> models = (List<Map<String, Object>>) objs.get("models");
        for (Map<String, Object> m : models) {
            Object v = m.get("model");
            if (v instanceof CodegenModel) {
                CodegenModel model = (CodegenModel) v;
                if (model.isEnum) {
                    continue;
                }

                for (CodegenProperty param : model.vars) {
                    if (!param.isNullable || param.isMapContainer || param.isListContainer) {
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

        // The superclass determines the list of required golang imports. The actual list of imports
        // depends on which types are used, which is done in the code above. So super.postProcessModels
        // must be invoked at the end of this method.
        objs = super.postProcessModels(objs);
        return objs;
    }
}
