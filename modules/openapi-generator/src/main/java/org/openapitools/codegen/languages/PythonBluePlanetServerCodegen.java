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

import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.meta.features.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.EnumSet;

public class PythonBluePlanetServerCodegen extends AbstractPythonConnexionServerCodegen {
    private final Logger LOGGER = LoggerFactory.getLogger(PythonBluePlanetServerCodegen.class);

    protected String modelDocPath = "";
    protected String modelTestPath = "";

    public PythonBluePlanetServerCodegen() {
        super("python-blueplanet", true);

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON, WireFormatFeature.XML, WireFormatFeature.Custom))
                .securityFeatures(EnumSet.of(
                        SecurityFeature.BasicAuth,
                        SecurityFeature.BearerToken,
                        SecurityFeature.ApiKey,
                        SecurityFeature.OAuth2_Implicit
                ))
                .excludeGlobalFeatures(
                        GlobalFeature.XMLStructureDefinitions,
                        GlobalFeature.Callbacks,
                        GlobalFeature.LinkObjects,
                        GlobalFeature.ParameterStyling
                )
                .excludeSchemaSupportFeatures(
                        SchemaSupportFeature.Polymorphism
                )
                .excludeParameterFeatures(
                        ParameterFeature.Cookie
                )
        );

        testPackage = "tests";
        embeddedTemplateDir = templateDir = "python-blueplanet";
    }


    /**
     * Configures a friendly name for the generator.  This will be used by the generator
     * to select the library with the -g flag.
     *
     * @return the friendly name for the generator
     */
    @Override
    public String getName() {
        return "python-blueplanet";
    }

    @Override
    public void processOpts() {
        super.processOpts();
        //apiTemplateFiles.clear();

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_NAME)) {
            setPackageName((String) additionalProperties.get(CodegenConstants.PACKAGE_NAME));
        } else {
            setPackageName("swagger_microservice");
            additionalProperties.put(CodegenConstants.PACKAGE_NAME, this.packageName);
        }
        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_VERSION)) {
            setPackageVersion((String) additionalProperties.get(CodegenConstants.PACKAGE_VERSION));
        } else {
            setPackageVersion("1.0.0");
            additionalProperties.put(CodegenConstants.PACKAGE_VERSION, this.packageVersion);
        }
        if (additionalProperties.containsKey(CONTROLLER_PACKAGE)) {
            this.controllerPackage = additionalProperties.get(CONTROLLER_PACKAGE).toString();
        } else {
            this.controllerPackage = "controllers";
            additionalProperties.put(CONTROLLER_PACKAGE, this.controllerPackage);
        }
        if (additionalProperties.containsKey(DEFAULT_CONTROLLER)) {
            this.defaultController = additionalProperties.get(DEFAULT_CONTROLLER).toString();
        } else {
            this.defaultController = "default_controller";
            additionalProperties.put(DEFAULT_CONTROLLER, this.defaultController);
        }
        if (Boolean.TRUE.equals(additionalProperties.get(SUPPORT_PYTHON2))) {
            additionalProperties.put(SUPPORT_PYTHON2, Boolean.TRUE);
            typeMapping.put("long", "long");
        }

        String APP_PATH = "app" + File.separatorChar;
        String APP_PACKAGE_PATH = APP_PATH + packageName;
        String TEST_PATH = APP_PACKAGE_PATH + File.separatorChar + "test";
        String MODEL_PATH = APP_PACKAGE_PATH + File.separatorChar + "models";
        String CONTROLLER_PATH = APP_PACKAGE_PATH + File.separatorChar + "controllers";
        String SOLUTION_PATH = "solution" + File.separatorChar;
        String SWAGGER_PATH = APP_PACKAGE_PATH + File.separatorChar + "swagger";

        // make solution and app src path available in mustache template
        additionalProperties.put("appSrcPath", APP_PATH);
        additionalProperties.put("solutionSrcPath", SOLUTION_PATH);
        additionalProperties.put("modelDefinitionsSrcPath", modelDocPath);

        /*
         * Supporting Files.  You can write single files for the generator with the
         * entire object tree available.  If the input file has a suffix of `.mustache
         * it will be processed by the template engine.  Otherwise, it will be copied
         */

        // Root Directory
        supportingFiles.add(new SupportingFile("Makefile.mustache", "", "Makefile"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("requirements.mustache", "", "requirements.txt"));

        // App Directory
        supportingFiles.add(new SupportingFile("app/Dockerfile.mustache", APP_PATH, "Dockerfile"));
        supportingFiles.add(new SupportingFile("app/dockerignore.mustache", APP_PATH, ".dockerignore"));
        supportingFiles.add(new SupportingFile("app/gitignore.mustache", APP_PATH, ".gitignore"));
        supportingFiles.add(new SupportingFile("app/README.mustache", APP_PATH, "README.md"));
        supportingFiles.add(new SupportingFile("app/requirements.mustache", APP_PATH, "requirements.txt"));
        supportingFiles.add(new SupportingFile("app/setup.mustache", APP_PATH, "setup.py"));
        supportingFiles.add(new SupportingFile("app/tox.mustache", APP_PATH, "tox.ini"));
        supportingFiles.add(new SupportingFile("app/test-requirements.mustache", APP_PATH, "test-requirements.txt"));

        supportingFiles.add(new SupportingFile("app/{{packageName}}/__init__.mustache", APP_PACKAGE_PATH, "__init__.py"));
        supportingFiles.add(new SupportingFile("app/{{packageName}}/__main__.mustache", APP_PACKAGE_PATH, "__main__.py"));
        supportingFiles.add(new SupportingFile("app/{{packageName}}/encoder.mustache", APP_PACKAGE_PATH, "encoder.py"));
        supportingFiles.add(new SupportingFile("app/{{packageName}}/util.mustache", APP_PACKAGE_PATH, "util.py"));
        supportingFiles.add(new SupportingFile("app/{{packageName}}/typing_utils.mustache", APP_PACKAGE_PATH, "typing_utils.py"));

        supportingFiles.add(new SupportingFile("app/{{packageName}}/controllers/__init__.mustache", CONTROLLER_PATH, "__init__.py"));

        supportingFiles.add(new SupportingFile("app/{{packageName}}/models/__init__.mustache", MODEL_PATH, "__init__.py"));
        supportingFiles.add(new SupportingFile("app/{{packageName}}/models/base_model_.mustache", MODEL_PATH, "base_model_.py"));

        supportingFiles.add(new SupportingFile("app/{{packageName}}/test/__init__.mustache", TEST_PATH, "__init__.py"));

        supportingFiles.add(new SupportingFile("app/{{packageName}}/swagger/swagger.mustache", SWAGGER_PATH, "swagger.yaml"));

        // Solution Directory
        supportingFiles.add(new SupportingFile("solution/fig.mustache", SOLUTION_PATH, "fig.yml"));

        // Dynamically generated file definitions
        modelTemplateFiles.remove("model.mustache", ".py");
        modelTemplateFiles.put("app/{{packageName}}/models/model.mustache", ".py");
        modelPackage = "app." + packageName + ".models";

        apiTemplateFiles.remove("controller.mustache", ".py");
        apiTemplateFiles.put("app/{{packageName}}/controllers/controller.mustache", ".py");
        controllerPackage = "app." + packageName + ".controllers";

        apiTestTemplateFiles().remove("controller_test.mustache", ".py");
        apiTestTemplateFiles.put("app/{{packageName}}/test/controller_test.mustache", ".py");
        testPackage = "app." + packageName + ".test";

        // hack: Use ModelDoc to generate tosca files  TODO: implement new Java class
        modelDocPath = "model-definitions.types.tosca." + packageName;
        modelDocTemplateFiles.put("model-definitions/types/tosca/{{packageName}}/{{model}}.mustache", ".tosca");

        // hack: Use ModelTest to generate ui files  TODO: implement new Java class
        modelTestTemplateFiles.put("model-definitions/types/ddui-views/{{packageName}}.resourceTypes.{{model}}/create.mustache", "create.json");
        modelTestTemplateFiles.put("model-definitions/types/ddui-views/{{packageName}}.resourceTypes.{{model}}/high.mustache", "high.json");
        modelTestTemplateFiles.put("model-definitions/types/ddui-views/{{packageName}}.resourceTypes.{{model}}/low.mustache", "low.json");
        modelTestPath = "model-definitions" + File.separator + "types";
    }

    @Override
    protected void addSupportingFiles() {
        String APP_PATH = "app" + File.separatorChar;
        String APP_PACKAGE_PATH = APP_PATH + packageName;
        String TEST_PATH = APP_PACKAGE_PATH + File.separatorChar + "test";
        String MODEL_PATH = APP_PACKAGE_PATH + File.separatorChar + "models";
        String CONTROLLER_PATH = APP_PACKAGE_PATH + File.separatorChar + "controllers";
        String SOLUTION_PATH = "solution" + File.separatorChar;
        String SWAGGER_PATH = APP_PACKAGE_PATH + File.separatorChar + "swagger";

        // TODO: PythonAbstract should allow override
        supportingFiles.clear();

        // make solution and app src path available in mustache template
        additionalProperties.put("appSrcPath", APP_PATH);
        additionalProperties.put("solutionSrcPath", SOLUTION_PATH);
        additionalProperties.put("modelDefinitionsSrcPath", modelDocPath);

        // Root Directory
        supportingFiles.add(new SupportingFile("Makefile.mustache", "", "Makefile"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("requirements.mustache", "", "requirements.txt"));

        // App Directory
        supportingFiles.add(new SupportingFile("app/Dockerfile.mustache", APP_PATH, "Dockerfile"));
        supportingFiles.add(new SupportingFile("app/dockerignore.mustache", APP_PATH, ".dockerignore"));
        supportingFiles.add(new SupportingFile("app/gitignore.mustache", APP_PATH, ".gitignore"));
        supportingFiles.add(new SupportingFile("app/README.mustache", APP_PATH, "README.md"));
        supportingFiles.add(new SupportingFile("app/requirements.mustache", APP_PATH, "requirements.txt"));
        supportingFiles.add(new SupportingFile("app/setup.mustache", APP_PATH, "setup.py"));
        supportingFiles.add(new SupportingFile("app/tox.mustache", APP_PATH, "tox.ini"));
        supportingFiles.add(new SupportingFile("app/test-requirements.mustache", APP_PATH, "test-requirements.txt"));

        supportingFiles.add(new SupportingFile("app/{{packageName}}/__init__.mustache", APP_PACKAGE_PATH, "__init__.py"));
        supportingFiles.add(new SupportingFile("app/{{packageName}}/__main__.mustache", APP_PACKAGE_PATH, "__main__.py"));
        supportingFiles.add(new SupportingFile("app/{{packageName}}/encoder.mustache", APP_PACKAGE_PATH, "encoder.py"));
        supportingFiles.add(new SupportingFile("app/{{packageName}}/util.mustache", APP_PACKAGE_PATH, "util.py"));
        supportingFiles.add(new SupportingFile("app/{{packageName}}/typing_utils.mustache", APP_PACKAGE_PATH, "typing_utils.py"));

        supportingFiles.add(new SupportingFile("app/{{packageName}}/controllers/__init__.mustache", CONTROLLER_PATH, "__init__.py"));

        supportingFiles.add(new SupportingFile("app/{{packageName}}/models/__init__.mustache", MODEL_PATH, "__init__.py"));
        supportingFiles.add(new SupportingFile("app/{{packageName}}/models/base_model_.mustache", MODEL_PATH, "base_model_.py"));

        supportingFiles.add(new SupportingFile("app/{{packageName}}/test/__init__.mustache", TEST_PATH, "__init__.py"));

        supportingFiles.add(new SupportingFile("app/{{packageName}}/swagger/swagger.mustache", SWAGGER_PATH, "swagger.yaml"));

        // Solution Directory
        supportingFiles.add(new SupportingFile("solution/fig.mustache", SOLUTION_PATH, "fig.yml"));
    }

    @Override
    public String modelDocFileFolder() {
        // character replaces should _only_ occur on paths we define. Don't replace on outputFolder (which is supplied by the user and should always be considered correct)
        return outputFolder + File.separator + modelDocPath.replace('.', File.separatorChar);
    }

    @Override
    public String toModelDocFilename( String name ) {
        return toModelName( name ) + "_ResourceType";
    }

    @Override
    public String modelTestFileFolder() {
        return outputFolder + File.separator + modelTestPath + File.separator + "ddui-views";
    }

    @Override
    public String toModelTestFilename(String name) {
        String resourceTypeFolder = packageName + ".resourceTypes." + toModelName(name) + File.separator;
        return resourceTypeFolder;
    }

    /**
     * Location to write api files.  You can use the apiPackage() as defined when the class is
     * instantiated
     */
    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + apiPackage().replace('.', File.separatorChar);
    }

    @Override
    public String generatorLanguageVersion() { return "2.7+ and 3.5.2+"; };
}
