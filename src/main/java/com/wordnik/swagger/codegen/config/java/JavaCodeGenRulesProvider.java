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

package com.wordnik.swagger.codegen.config.java;

import com.wordnik.swagger.codegen.config.RulesProvider;

import java.util.ArrayList;
import java.util.List;

/**
 * User: ramesh
 * Date: 5/31/11
 * Time: 7:04 AM
 */
public class JavaCodeGenRulesProvider implements RulesProvider {

    private List<String> ignoreMethods = new ArrayList<String>();
    private List<String> ignoreModels = new ArrayList<String>();

    public JavaCodeGenRulesProvider() {

    }

    public boolean isMethodIgnored(String serviceName, String methodName){
        return (ignoreMethods.contains(serviceName+"."+methodName));
    }

    public boolean isModelIgnored(String modelName) {
        return ignoreModels.contains(modelName);
    }

    public List<String> getIgnoreMethods() {
        return ignoreMethods;
    }

    public void setIgnoreMethods(List<String> ignoreMethods) {
        this.ignoreMethods = ignoreMethods;
    }

    public List<String> getIgnoreModels() {
        return ignoreModels;
    }

    public void setIgnoreModels(List<String> ignoreModels) {
        this.ignoreModels = ignoreModels;
    }

}
