/**
 *  Copyright 2011 Wordnik, Inc.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package com.wordnik.swagger.runtime.common;

import java.util.Map;

/**
 * User: ramesh
 * Date: 8/4/11
 * Time: 6:39 PM
 */
public class ApiKeyAuthTokenBasedSecurityHandler implements SecurityHandler {

    private String apiKey = "";
    private String authToken = "";

    public ApiKeyAuthTokenBasedSecurityHandler(String apiKey, String authToken) {
        this.apiKey = apiKey;
        this.authToken = authToken;
    }

    public String getAuthToken() {
        return authToken;
    }

    public void setAuthToken(String authToken) {
        this.authToken = authToken;
    }

    public String getApiKey() {
        return apiKey;
    }

    public void setApiKey(String apiKey) {
        this.apiKey = apiKey;
    }

    /**
     * Populate the security infomration in http headers map and/or reqsource URL.
     *
     * Value spopulated in the http headers map will be set as http headers while making the server communication.
     *
     * Depending on the usecase requried information can be added to either of them or both.
     *
     * @param resourceURL
     * @param headers
     */
    public void populateSecurityInfo(String resourceURL, Map<String, String> httpHeaders) {
        resourceURL = resourceURL + "api_key="+apiKey;
        httpHeaders.put("api_key", apiKey);
        httpHeaders.put("auth_token", authToken);
    }
}
