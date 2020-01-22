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

import org.openapitools.codegen.SupportingFile;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;

public class PythonFlaskConnexionServerCodegen extends PythonAbstractConnexionServerCodegen {
    private static final Logger LOGGER = LoggerFactory.getLogger(PythonFlaskConnexionServerCodegen.class);

    public PythonFlaskConnexionServerCodegen() {
        super("python-flask", false);
    }

    /**
     * Configures a friendly name for the generator.  This will be used by the generator
     * to select the library with the -g flag.
     *
     * @return the friendly name for the generator
     */
    @Override
    public String getName() {
        return "python-flask";
    }

    @Override
    protected void addSupportingFiles() {
        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));
        supportingFiles.add(new SupportingFile("Dockerfile.mustache", "", "Dockerfile"));
        supportingFiles.add(new SupportingFile("dockerignore.mustache", "", ".dockerignore"));
        supportingFiles.add(new SupportingFile("setup.mustache", "", "setup.py"));
        supportingFiles.add(new SupportingFile("tox.mustache", "", "tox.ini"));
        supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));
        supportingFiles.add(new SupportingFile("travis.mustache", "", ".travis.yml"));
        supportingFiles.add(new SupportingFile("encoder.mustache", packagePath(), "encoder.py"));
        supportingFiles.add(new SupportingFile("__init__test.mustache", packagePath() + File.separatorChar + testPackage, "__init__.py"));
        supportingFiles.add(new SupportingFile("__init__.mustache", packagePath(), "__init__.py"));
        testPackage = packageName + "." + testPackage;
    }
}
