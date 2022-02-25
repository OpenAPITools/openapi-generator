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
import org.openapitools.codegen.meta.features.annotations.ToolingExtension;

/**
 * Defines wire formats explicitly defined in spec or supported by the tool.
 */
public enum WireFormatFeature {
    /**
     * Supports JSON transfer
     */
    @OAS2 @OAS3
    JSON,

    /**
     * Supports XML transfer
     */
    @OAS2 @OAS3
    XML,

    /**
     * Supports protocol buffer transfer
     */
    @ToolingExtension
    PROTOBUF,

    /**
     * Supports other mime types or wire formats for transfer, to be documented by generators.
     */
    @OAS2 @OAS3
    Custom
}
