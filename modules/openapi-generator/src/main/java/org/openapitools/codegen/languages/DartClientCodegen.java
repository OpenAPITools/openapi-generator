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

import org.openapitools.codegen.CliOption;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.SupportingFile;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.HashMap;
import java.util.Map;

public class DartClientCodegen extends AbstractDartCodegen {

    private static final Logger LOGGER = LoggerFactory.getLogger(DartClientCodegen.class);

    public DartClientCodegen() {
        super();

        CliOption serializationLibrary = new CliOption(CodegenConstants.SERIALIZATION_LIBRARY, "Serialization library, by default uses custom generator");
        Map<String, String> serializationOptions = new HashMap<>();
        serializationOptions.put(SERIALIZATION_LIBRARY_CUSTOM, "Use custom generator as serialization library");
        serializationOptions.put(SERIALIZATION_LIBRARY_JSON_SERIALIZABLE, "Use json_serializable as serialization library");
        serializationLibrary.setEnum(serializationOptions);
        cliOptions.add(serializationLibrary);
    }

    @Override
    public void processOpts() {
        super.processOpts();
        this.setSerializationLibrary();
    }

    private void setSerializationLibrary() {
        final String serialization_library = getLibrary();
        LOGGER.info("Using serialization library {}", serialization_library);

        switch (serialization_library) {
            case SERIALIZATION_LIBRARY_JSON_SERIALIZABLE:
                additionalProperties.put(SERIALIZATION_LIBRARY_JSON_SERIALIZABLE, "true");
                // json_serializable requires build.yaml
                supportingFiles.add(new SupportingFile("build.yaml.mustache",
                        "" /* main project dir */,
                        "build.yaml"));
                break;

            case SERIALIZATION_LIBRARY_CUSTOM: // fall trough to default backwards compatible generator
            default:
                additionalProperties.put(SERIALIZATION_LIBRARY_CUSTOM_SERIALIZATION, "true");

        }
    }
}
