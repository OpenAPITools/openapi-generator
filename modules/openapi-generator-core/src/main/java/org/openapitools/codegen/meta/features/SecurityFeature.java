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
 * Defines security features supported in the generated code.
 */
public enum SecurityFeature {
    /**
     * Supports header-based basic http auth.
     */
    @OAS2 @OAS3
    BasicAuth,

    /**
     * Supports header-based api-key http auth.
     */
    @OAS2 @OAS3
    ApiKey,

    /**
     * Supports openid connect based http auth. Implies a requirement on openIdConnectUrl.
     */
    @OAS3
    OpenIDConnect,

    /**
     * Supports header-based bearer auth (e.g. header + bearer format).
     */
    @OAS3
    BearerToken,

    /**
     * Supports authorization via OAuth2 implicit flow.
     */
    @OAS2 @OAS3
    OAuth2_Implicit,

    /**
     * Supports authorization via OAuth2 password flow.
     */
    @OAS2 @OAS3
    OAuth2_Password,

    /**
     * Supports authorization via OAuth2 client credentials flow ("application" in OAS 2.0).
     *
     * <p>In OAS 2.0, this is called "application" flow.</p>
     */
    @OAS2 @OAS3
    OAuth2_ClientCredentials,

    /**
     * Supports authorization via OAuth2 flow ("accessCode" in OAS 2.0).
     *
     * <p>In OAS 2.0, this is called "accessCode" flow.</p>
     */
    @OAS2 @OAS3
    OAuth2_AuthorizationCode
}
