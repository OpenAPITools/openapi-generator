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
 * Defines a set of globally available features. That is, support of these are often defined at the top-level of the spec, or
 * defines general support of a feature (e.g. Examples, XMLStructureDefinitions).
 */
public enum GlobalFeature {
    /**
     * Supports specifying the host or ip of the target system. If not defined, this should fall back to the
     * host/ip (and optional port) of the server which delivered the spec document.
     */
    @OAS2 @OAS3
    Host,

    /**
     * Supports providing an API prefix, appended to the host.
     *
     * <p>OAS 3.x supports this indirectly via servers with template variables.</p>
     */
    @OAS2 @OAS3
    BasePath,

    /**
     * Supports passing information about the target server to the client.
     *
     * <p>
     * Information passed to generated code should be explicitly documented in a generator's README.
     * </p>
     */
    @OAS2 @OAS3
    Info,

    /**
     * Supports customization of the scheme "http", "https", "ws", "wss".
     *
     * <p>
     * If a generator only supports partial schemes, please choose the PartialSchemes option.
     * </p>
     *
     * <p>OAS 3.x supports this indirectly via servers with template variables.</p>
     */
    @OAS2 @OAS3
    Schemes,

    /**
     * Supports fewer than all schemes supported by OpenAPI Specification.
     *
     * <p>
     * Support should be explicitly documented in a generator's README.
     * </p>
     *
     * <p>OAS 3.x supports this indirectly via servers with template variables.</p>
     */
    @OAS2 @OAS3
    PartialSchemes,

    /**
     * Supports a globally defined array of consumable MimeTypes.
     *
     * <p>Global support is undefined in OAS 3.x.</p>
     */
    @OAS2
    Consumes,

    /**
     * Supports a globally defined array of produced MimeTypes.
     *
     * <p>Global support is undefined in OAS 3.x.</p>
     */
    @OAS2
    Produces,

    /**
     * Exposes external documentation defined in the specification document to generated code.
     */
    @OAS2 @OAS3
    ExternalDocumentation,

    /**
     * Allows the ability to provide example input/output structures, usually in JSON format.
     */
    @OAS2 @OAS3
    Examples,

    /**
     * Differs from supporting the MimeType.XML feature, in that this option indicates whether XML structures can be defined by spec document and honored by the caller.
     */
    @OAS2 @OAS3
    XMLStructureDefinitions,

    /**
     * Supports targeting one or more servers.
     *
     * <p>
     * That is, server is not hard-coded (although there may be a default).
     * This option is valid only for "servers" without open-ended values.
     * </p>
     */
    @OAS3
    MultiServer,

    /**
     * Supports targeting one or more servers, PLUS the ability to provide values for templated server parts
     */
    @OAS3
    ParameterizedServer,

    /**
     * Supports OAS 3.x "style" for parameters.
     *
     * <p>
     * NOTE: This option is more relevant for documentation generators which support HTML stylesheets, but may be used
     *       to determine structural characteristics of a property (as with OAS 3.x lack of collectionFormat).
     * </p>
     */
    @OAS3
    ParameterStyling,

    /**
     * Supports OAS 3.x callbacks.
     */
    @OAS3
    Callbacks,

    /**
     * Supports OAS 3.x link objects, but does *NOT* suggest generated clients auto-follow links.
     */
    @OAS3
    LinkObjects
}
