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

package com.wordnik.swagger.common;

import java.util.Map;

/**
 * Provide methods that are responsible for handling security aspect while communicating with the backend server.
 *
 * Example: For some cases API key may need to be passed in the headers for all server communication and some times
 * user authentication token may need to be passed along with api key.
 *
 * Implementers of this class are responsible for handling storing information related to secutiry and sending it
 * along with all API calls
 *
 * User: ramesh
 * Date: 4/12/11
 * Time: 5:46 PM
 */
public interface SecurityHandler {

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
    public void populateSecurityInfo(String resourceURL, Map<String, String> httpHeaders);
}