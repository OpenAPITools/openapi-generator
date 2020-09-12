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
 * Defines common data types supported by a generator.
 * Some of these features are defined in specs, and some are specific to the tool.
 *
 * Where data types are listed as tool-specific, this either indicates that the data type is common enough that it is an officially
 * supported custom data type by the toolset (see {@link DataTypeFeature#Decimal}), or that the consideration of a special type isn't
 * explicitly mentioned by the specification(s) but differs enough across languages that it warrants a special callout (see {@link DataTypeFeature#ArrayOfModel}).
 */
public enum DataTypeFeature {
    /**
     * Supports a generator-specific support usually via type=string's format property (e.g. email, uuid, etc), should be documented in generator README.
     *
     * <p>Loosely described in OpenAPI Specification(s). Generally means a custom "format" option applied to a string-typed property.</p>
     */
    @OAS2 @OAS3
    Custom,

    /**
     * Supports integer/int32
     */
    @OAS2 @OAS3
    Int32,

    /**
     * Supports integer/int64
     */
    @OAS2 @OAS3
    Int64,

    /**
     * Supports number/float
     */
    @OAS2 @OAS3
    Float,

    /**
     * Supports number/double
     */
    @OAS2 @OAS3
    Double,

    /**
     * Supports number/decimal (a special case for some languages)
     *
     * <p>Decimal is not a type defined by OAS 2.0 specification</p>
     */
    @ToolingExtension
    Decimal,

    /**
     * Supports string
     */
    @OAS2 @OAS3
    String,

    /**
     * Supports string/byte: base64 encoded
     */
    @OAS2 @OAS3
    Byte,

    /**
     * Supports string/binary: any collection of octets
     */
    @OAS2 @OAS3
    Binary,

    /**
     * Supports boolean
     */
    @OAS2 @OAS3
    Boolean,

    /**
     * Supports string/date: full-date RFC3339
     *
     * @see <a href="https://tools.ietf.org/html/rfc3339">RFC3339</a>
     */
    @OAS2 @OAS3
    Date,

    /**
     * Supports string/date-time: date-time RFC3339
     *
     * @see <a href="https://tools.ietf.org/html/rfc3339">RFC3339</a>
     */
    @OAS2 @OAS3
    DateTime,

    /**
     * Supports string/password: A hint to UIs to obscure input.
     *
     *
     * <p>
     *     This should be used as an indicator for password best practices, such as assigning a variable to
     *     a character array rather than string, avoiding logging the variable in clear text, and masking the value
     *     in any user inputs. See OWASP for best practices.
     * </p>
     */
    @OAS2 @OAS3
    Password,


    /**
     * Supports file inputs (e.g. multipart support).
     *
     * <p>OAS 3.x defines files differently.</p>
     * <p>
     * OAS 3.x does not have an explicit "file" type and instead relies on ContentType or response types.
     * That's not to say a generator doesn't support files, only that there's no direct
     * "file" type defined in the spec document.
     * </p>
     * <p>
     * NOTE: The default workflow may provide an "isFile" helper or synthesize the assumptions around files in the case of OAS 3.x.
     * </p>
     */
    @OAS2
    File,

    /**
     * Supports arrays of data
     */
    @OAS2 @OAS3
    Array,

    /**
     * Supports map of data
     */
    @ToolingExtension
    Maps,

    /**
     * Supports specifying the format of the array if type array is used (one of: csv, ssv, tsv, pipes).
     *
     * <p>
     * For multi support, check {@link DataTypeFeature#CollectionFormatMulti}. OAS 3.x removes collectionFormat in favor of Style properties.
     * </p>
     */
    @OAS2
    CollectionFormat,

    /**
     * Supports collection format=multi.
     *
     * <p>
     * This is special cased because it is not as easily implemented as a delimiter as with CollectionFormat.
     * OAS 3.x removes collectionFormat for style properties.
     * </p>
     */
    @OAS2
    CollectionFormatMulti,

    /**
     * Supports enum properties
     */
    @OAS2 @OAS3
    Enum,

    /**
     * Supports an array of enum
     */
    @ToolingExtension
    ArrayOfEnum,

    /**
     * Supports an array of models
     */
    @ToolingExtension
    ArrayOfModel,

    /**
     * Supports an array of arrays (primitives)
     */
    @ToolingExtension
    ArrayOfCollectionOfPrimitives,

    /**
     * Supports an array of arrays (models)
     */
    @ToolingExtension
    ArrayOfCollectionOfModel,

    /**
     * Supports an array of arrays (enums)
     */
    @ToolingExtension
    ArrayOfCollectionOfEnum,

    /**
     * Supports a map of enums
     */
    @ToolingExtension
    MapOfEnum,

    /**
     * Supports a map of models
     */
    @ToolingExtension
    MapOfModel,

    /**
     * Supports a map of arrays (primitives)
     */
    @ToolingExtension
    MapOfCollectionOfPrimitives,

    /**
     * Supports a map of arrays (models)
     */
    @ToolingExtension
    MapOfCollectionOfModel,

    /**
     * Supports a map of arrays (enums)
     */
    @ToolingExtension
    MapOfCollectionOfEnum
}
