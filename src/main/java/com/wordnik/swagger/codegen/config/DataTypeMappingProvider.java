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

package com.wordnik.swagger.codegen.config;

import java.util.List;

/**
 * Implementations of this class is responsible for generating mapping between resource documentation data types and language
 * specific data type
 * 
 * User: ramesh
 * Date: 5/27/11
 * Time: 7:39 AM
 */
public interface DataTypeMappingProvider {

    /**
     * Checks nature of data type.
     *
     * This is needed in generating return values, input and model class generations.
     *
     * Example: in java <Code>String</Code>, <Code>Integer</Code>, <Code>Boolean</Code> are considered as primitive
     * types
     * @param type
     * @return
     */
    public boolean isPrimitiveType(String type);

    /**
     * Signature that should be used when returning list of given object type.
     *
     * Example: in java this output will look as <Code> List<User> </Code> for methods that returns a list of user objects
     * @param typeClass of class that list object contains.
     * @return
     */
    public String getListReturnTypeSignature(String typeClass);

    /**
     * Signature that should be used when returning map of given object type.
     *
     * Example: in java this output will look as <Code> Map<User> </Code> for methods that returns maps
     * @param typeClass of class that list object contains.
     * @return
     */
    public String getMapReturnTypeSignature(String typeClass);

    /**
     * Signature that should be used when returning set of given object type.
     *
     * Example: in java this output will look as <Code> Set<User> </Code> for methods that returns a set of user objects
     * @param typeClass of class that the set object contains.
     * @return
     */
    public String getSetReturnTypeSignature(String typeClass);

    /**
     * Initialization need for list objects. Example. If it is java list the initialization will look as
     *
     * <Code>
     *      new ArrayList<ClassName>()
     * </Code>
     *
     * @param typeClass
     * @return
     */
    public String generateListInitialization(String typeClass);

    /**
     * Initialization need for map objects. Example. If it is java map the initialization will look as
     *
     * <Code>
     *      new HashMap<ClassName>()
     * </Code>
     *
     * @param typeClass
     * @return
     */
    public String generateMapInitialization(String typeClass);

    /**
     * Initialization need for set objects. Example. If it is java set the initialization will look as
     *
     * <Code>
     *      new HashSet<ClassName>()
     * </Code>
     *
     * @param typeClass
     * @return
     */
    public String generateSetInitialization(String typeClass);

    /**
     * Sets variable initialization.
     *
     * Example: In scala initializing a variable with an unknown value will be:
     * <Code>
     *      var age:String = _
     * </Code>
     * @return
     */
    public String generateVariableInitialization(String typeClass);
   
    /**
     * Gets list of items that needs to be included when referring list objects in model or resource classes.
     *
     * Example: In java this information is used as java imports. In java while using lists we use an interface of
     * <Code>List</Code> and implementation of <Code>ArrayList</Code>. So the the implementation of this method in java
     * language will be:
     * <Code>
     *      List<String> imports = new ArrayList<String>();
            imports.add("java.util.List");
            imports.add("java.util.ArrayList");
     * </Code>
     * @return
     */
    public List<String> getListIncludes();

    /**
     * Gets list of items that needs to be included when referring map objects in model or resource classes.
     *
     * Example: In java this information is used as java imports. In java while using map we use an interface of
     *  <Code>Map</Code> and implementation of <Code>HashMap</Code>. So the the implementation of this method in java
     * language will be:
     * <Code>
     *      List<String> imports = new ArrayList<String>();
            imports.add("java.util.Map");
            imports.add("java.util.HashMap");
     * </Code>
     * @return
     */
    public List<String> getMapIncludes();

    /**
     * Gets list of items that needs to be included when referring set objects in model or resource classes.
     *
     * Example: in java while using sets we use an interface of <Code>Set</Code> and implementation of
     * <Code>HashSet</Code>. So the the implementation of this method in java
     * language will be:
     * <Code>
     *      List<String> imports = new ArrayList<String>();
            imports.add("java.util.Set");
            imports.add("java.util.HashSet");
     * </Code>
     * @return
     */
    public List<String> getSetIncludes();

    /**
     * Gets list of items that needs to be included when referring date objects in model or resource classes.
     *
     * Example: in java while using Data we use <Code> java.util.Date</Code>. So the output will as follows:
     * <Code>
     *      List<String> imports = new ArrayList<String>();
            imports.add("java.util.Date");
     * </Code>
     * @return
     */
    public List<String> getDateIncludes();

    /**
     * Class type definition for a given input.
     *
     * Example: In java language:  For inputs Integer and primitive true, the class type will be int, if primitiveObject is false
     * the class type will be Integer. For inputs user the class type will be User as the input object is not primitive.
     * for input List[user] the class type will be <Code> List<User> </Code> . For input Map[int, String] the equivalent java
     * translation will be <Code> Map<Integer, String> </Code>
     *
     * @param type
     * @param primitiveObject This argument used to indicate, if the given input type is primitive,
     * should we return primitive types or primitive classes.
     * @return
     */
    public String getClassType(String type, boolean primitiveObject);

    /**
     * If the class contains generics then this will return type of generics object else returns same object
     *
     * Example: If the resource documentation says return type as List[User] the equivalent generic type for java will be
     *
     * <Code> User </Code>
     *
     * If the input is Map[int] the equivalent java translation will be <Code> Int </Code>
     * @param type
     * @return
     */
    public String getGenericType(String type);

    /**
     * Returns the syntax for defintion of an object of type and name
     *
     * @param argumentType
     * @param argumentName
     * @return
     */
    public String getArgumentDefinition(String argumentType, String argumentName);

}
