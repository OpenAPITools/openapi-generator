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

import com.google.common.collect.Sets;
import org.openapitools.codegen.CliOption;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.SupportingFile;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

public class DartClientCodegen extends AbstractDartCodegen {

    private final Logger LOGGER = LoggerFactory.getLogger(DartClientCodegen.class);

    public static final String SERIALIZATION_LIBRARY_NATIVE = "native_serialization";

    public DartClientCodegen() {
        super();

        final CliOption serializationLibrary = CliOption.newString(CodegenConstants.SERIALIZATION_LIBRARY,
                "Specify serialization library");
        serializationLibrary.setDefault(SERIALIZATION_LIBRARY_NATIVE);

        final Map<String, String> serializationOptions = new HashMap<>();
        serializationOptions.put(SERIALIZATION_LIBRARY_NATIVE, "Use native serializer, backwards compatible");
        serializationLibrary.setEnum(serializationOptions);
        cliOptions.add(serializationLibrary);
    }

    @Override
    public void processOpts() {
        super.processOpts();

        // handle library not being set
        if(additionalProperties.get(CodegenConstants.SERIALIZATION_LIBRARY) == null) {
            this.library = SERIALIZATION_LIBRARY_NATIVE;
            LOGGER.debug("Serialization library not set, using default {}", SERIALIZATION_LIBRARY_NATIVE);
        } else {
            this.library = additionalProperties.get(CodegenConstants.SERIALIZATION_LIBRARY).toString();
        }

        this.setSerializationLibrary();

        final String libFolder = sourceFolder + File.separator + "lib";
        supportingFiles.add(new SupportingFile("pubspec.mustache", "", "pubspec.yaml"));
        supportingFiles.add(new SupportingFile("analysis_options.mustache", "", "analysis_options.yaml"));
        supportingFiles.add(new SupportingFile("api_client.mustache", libFolder, "api_client.dart"));
        supportingFiles.add(new SupportingFile("api_exception.mustache", libFolder, "api_exception.dart"));
        supportingFiles.add(new SupportingFile("api_helper.mustache", libFolder, "api_helper.dart"));
        supportingFiles.add(new SupportingFile("apilib.mustache", libFolder, "api.dart"));

        final String authFolder = sourceFolder + File.separator + "lib" + File.separator + "auth";
        supportingFiles.add(new SupportingFile("auth/authentication.mustache", authFolder, "authentication.dart"));
        supportingFiles.add(new SupportingFile("auth/http_basic_auth.mustache", authFolder, "http_basic_auth.dart"));
        supportingFiles.add(new SupportingFile("auth/http_bearer_auth.mustache", authFolder, "http_bearer_auth.dart"));
        supportingFiles.add(new SupportingFile("auth/api_key_auth.mustache", authFolder, "api_key_auth.dart"));
        supportingFiles.add(new SupportingFile("auth/oauth.mustache", authFolder, "oauth.dart"));
        supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));
        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("travis.mustache", "", ".travis.yml"));

    }

    private void setSerializationLibrary() {
        final String serialization_library = getLibrary();
        LOGGER.info("Using serialization library {}", serialization_library);

        switch (serialization_library) {
            case SERIALIZATION_LIBRARY_NATIVE: // fall through to default backwards compatible generator
            default:
                additionalProperties.put(SERIALIZATION_LIBRARY_NATIVE, "true");

        }
    }
}
