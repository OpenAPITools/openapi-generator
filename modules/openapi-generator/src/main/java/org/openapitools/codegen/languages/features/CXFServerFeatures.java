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

package org.openapitools.codegen.languages.features;

/**
 * Features supported by CXF 3 server
 */
public interface CXFServerFeatures
        extends CXFFeatures, SwaggerFeatures, SpringFeatures, JbossFeature, BeanValidationExtendedFeatures, SwaggerUIFeatures {

    public static final String USE_WADL_FEATURE = "useWadlFeature";

    public static final String USE_MULTIPART_FEATURE = "useMultipartFeature";

    public static final String ADD_CONSUMES_PRODUCES_JSON = "addConsumesProducesJson";

    public static final String USE_ANNOTATED_BASE_PATH = "useAnnotatedBasePath";

    public static final String GENERATE_NON_SPRING_APPLICATION = "generateNonSpringApplication";

    public static final String LOAD_TEST_DATA_FROM_FILE = "loadTestDataFromFile";

    public void setUseWadlFeature(boolean useWadlFeature);

    public void setUseMultipartFeature(boolean useMultipartFeature);

    public void setAddConsumesProducesJson(boolean addConsumesProducesJson);

    public void setUseAnnotatedBasePath(boolean useAnnotatedBasePath);

    public void setGenerateNonSpringApplication(boolean generateNonSpringApplication);
}
