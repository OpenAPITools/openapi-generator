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

/**
 * User: ramesh
 * Date: 10/18/11
 * Time: 11:35 PM
 */
public class AllowableRangeValues extends AllowableValues {

    private String valueType = "RANGE";

    private boolean inclusive = true;

    private String min;

    private String max;

    public String getValueType() {
        return valueType;
    }

    public void setValueType(String valueType) {
        this.valueType = valueType;
    }

    public String getMin() {
        return min;
    }

    public void setMin(String minValue) {
        this.min = minValue;
    }

    public String getMax() {
        return max;
    }

    public void setMax(String maxValue) {
        this.max = maxValue;
    }

    public boolean isInclusive() {
        return inclusive;
    }

    public void setInclusive(boolean inclusive) {
        this.inclusive = inclusive;
    }

    @Override
    public String toString() {
        if(inclusive){
            return "range(" + min + "," + max + ")";
        }else{
            return "rangeExclusive(" + min + "," + max + ")";
        }
    }

}
