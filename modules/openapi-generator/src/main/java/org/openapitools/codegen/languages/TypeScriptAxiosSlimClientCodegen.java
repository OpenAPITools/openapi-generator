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

public class TypeScriptAxiosSlimClientCodegen extends TypeScriptAxiosClientCodegen {

    public TypeScriptAxiosSlimClientCodegen() {
        super();
        outputFolder = "generated-code/typescript-axios-slim";
        embeddedTemplateDir = templateDir = "typescript-axios-slim";
        additionalProperties.put(USE_SINGLE_REQUEST_PARAMETER, true);
    }

    @Override
    public void processOpts() {
        additionalProperties.put(USE_SINGLE_REQUEST_PARAMETER, true);
        super.processOpts();
        additionalProperties.put(USE_SINGLE_REQUEST_PARAMETER, true);
    }

    @Override
    public String getName() {
        return "typescript-axios-slim";
    }

    @Override
    public String getHelp() {
        return "Generates a TypeScript client library using axios (slim direct API style with valibot validation).";
    }
}
