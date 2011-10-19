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

package com.wordnik.swagger.codegen.resource;

import java.util.List;

/**
 * User: ramesh
 * Date: 10/18/11
 * Time: 11:34 PM
 */
public class AllowableListValues extends AllowableValues {

    private String valueType = "LIST";

    private List<String> values;

    public String getValueType() {
        return valueType;
    }

    public void setValueType(String valueType) {
        this.valueType = valueType;
    }

    public List<String> getValues() {
        return values;
    }

    public void setValues(List<String> values) {
        this.values = values;
    }

    @Override
    public String toString() {
        StringBuffer buffer = new StringBuffer();
        if(this.getValues() != null){
            for(String value : values ){
                buffer.append(value);
                buffer.append(",");
            }
        }
        return buffer.toString().substring(0, buffer.toString().length()-1);
    }
}
