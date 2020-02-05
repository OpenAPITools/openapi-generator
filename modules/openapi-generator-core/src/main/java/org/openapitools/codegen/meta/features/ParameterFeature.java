/*
 * Copyright 2019 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

package org.openapitools.codegen.meta.features;

import org.openapitools.codegen.meta.features.annotations.OAS2;
import org.openapitools.codegen.meta.features.annotations.OAS3;

/**
 * Defines parameters supported by endpoints in the generated code.
 */
public enum ParameterFeature {
    /**
     * Supports path parameters.
     */
    @OAS2 @OAS3
    Path,

    /**
     * Supports query parameters.
     */
    @OAS2 @OAS3
    Query,

    /**
     * Supports header parameters.
     */
    @OAS2 @OAS3
    Header,

    /**
     * Supports body parameters.
     *
     * <p>
     *     OAS 3.x specification supports this structurally rather than as an "in" parameter.
     * </p>
     */
    @OAS2
    Body,

    /**
     * Supports form encoded parameters.
     *
     * OAS 3.x specification supports this structurally via content types rather than as an "in" parameter.
     */
    @OAS2
    FormUnencoded,

    /**
     * Supports multipart parameters.
     *
     * <p>OAS 3.x specification supports this structurally via content types rather than as an "in" parameter.</p>
     */
    @OAS2
    FormMultipart,

    /**
     * Supports Cookie parameters.
     *
     * <p>Not defined in OAS 2.0 and no tooling extensions currently supported for OAS 2.0 support.</p>
     */
    @OAS3
    Cookie
}
