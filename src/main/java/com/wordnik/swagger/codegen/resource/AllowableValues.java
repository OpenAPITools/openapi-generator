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

import org.codehaus.jackson.annotate.JsonSubTypes;
import org.codehaus.jackson.annotate.JsonTypeInfo;

import java.util.ArrayList;
import java.util.List;

/**
 * Common interface between range and list allowable values
 * User: ramesh
 * Date: 10/18/11
 * Time: 11:34 PM
 */
@JsonTypeInfo(
    use = JsonTypeInfo.Id.NAME,
    include = JsonTypeInfo.As.PROPERTY,
    property = "valueType")
@JsonSubTypes({
    @JsonSubTypes.Type(value = AllowableListValues.class, name = "LIST"),
    @JsonSubTypes.Type(value = AllowableRangeValues.class, name = "RANGE")
    })
public abstract class AllowableValues {

    public abstract String getValueType();

    public static AllowableValues ConvertAllowableValuesStringToObject(String data) {
         if(data != null){
            if(data.toLowerCase().startsWith("range")){
                AllowableRangeValues av = new AllowableRangeValues();
                String[] values = null;
                if(data.toLowerCase().startsWith("rangeexclusive")){
                    values = data.substring(15, data.length()-1).split(",");
                    av.setInclusive(false);
                }else{
                    values = data.substring(6, data.length()-1).split(",");
                    av.setInclusive(true);
                }
                av.setMin(values[0]);
                av.setMin(values[1]);
                return av;
            }else{
                List<String> allowedValues = new ArrayList<String>();
                if (data != null) {
                    String[] tokens = data.split(",");
                    for(String token: tokens){
                        allowedValues.add(token);
                    }
                }
                AllowableListValues av = new AllowableListValues();
                av.setValues(allowedValues);
                return av;
            }
        }
        return null;
    }
}
