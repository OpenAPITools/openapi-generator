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

import org.openapitools.codegen.meta.features.annotations.ToolingExtension;

/**
 * Defines a general set of modifications supported by a generated client.
 */
public enum ClientModificationFeature {
    /**
     * Supports defining a custom overall base path in generated client output.
     */
    @ToolingExtension
    BasePath,

    /**
     * Supports customizing authorizations in generated client output.
     */
    @ToolingExtension
    Authorizations,

    /**
     * Supports customizing the user agent in generated client output.
     */
    @ToolingExtension
    UserAgent,

    /**
     * Supports mock server. This feature disabled by default.
     */
    @ToolingExtension
    MockServer
}
