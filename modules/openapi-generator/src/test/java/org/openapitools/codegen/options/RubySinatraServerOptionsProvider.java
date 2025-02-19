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

package org.openapitools.codegen.options;

import com.google.common.collect.ImmutableMap;

import java.util.Map;

public class RubySinatraServerOptionsProvider implements OptionsProvider {
    @Override
    public String getLanguage() {
        return "ruby-sinatra";
    }

    @Override
    public Map<String, String> createOptions() {
        //SinatraServerCodegen doesn't have its own options and base options are cleared
        return ImmutableMap.of();
    }

    @Override
    public boolean isServer() {
        return true;
    }
}
