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
 * Defines special circumstances handled by the generator.
 */
public enum SchemaSupportFeature {
    /**
     * Support of simple schemas (those which define properties directly).
     */
    @OAS2 @OAS3
    Simple,

    /**
     * Support of complex schemas (those which refer to the properties of another model).
     *
     * <p>In OpenAPI Specification, this indicates support of AllOf/OneOf.</p>
     */
    @OAS2 @OAS3
    Composite,

    /**
     * Support for polymorphic classes.
     *
     * <p>
     * This suggests Composite support, but may not always be the case and is therefore separate.
     * </p>
     *
     * <p>In OpenAPI Specification, this indicates support of AllOf with a discriminator property on the derived schema.</p>
     */
    @OAS2 @OAS3
    Polymorphism,

    /**
     * Support for a union type.
     *
     * <p>
     * This means that a single "Type" in generated code may refer to one of any type in a set of 2 or more types.
     *
     * This is defined as a union as "OneOf" support is not explicitly limited to physical boundaries in OpenAPI Specification. The
     * implementation of such a type is easily represented dynamically (a JSON object), but requires explicit language support and
     * potentially a custom implementation (typed instances).
     *
     * Note that a generator may support "Unions" very loosely by returning an Object/Any/ref/interface{} type, leaving onus
     * on type determination to the consumer. This does *NOT* suggest generated code implements a "Union Type".
     * </p>
     *
     * <p>This suggests support of OneOf in OpenAPI Specification with a discriminator.</p>
     */
    @OAS3
    Union
}
