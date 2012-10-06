/**
 *  Copyright 2012 Wordnik, Inc.
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

package com.wordnik.swagger.model;

import javax.xml.bind.annotation.*;

import com.fasterxml.jackson.annotation.*;

import static com.fasterxml.jackson.annotation.JsonTypeInfo.*;

/**
 * forgive me lord, for I used Java
 * @SeeAlso https://issues.scala-lang.org/browse/SI-5165
 **/
@JsonTypeInfo(use=Id.NAME, include=As.PROPERTY, property="valueType")
@JsonSubTypes({
    @JsonSubTypes.Type(value=AllowableListValues.class, name="LIST"),
    @JsonSubTypes.Type(value=AllowableRangeValues.class, name="RANGE")
})  
public abstract class AllowableValues {}