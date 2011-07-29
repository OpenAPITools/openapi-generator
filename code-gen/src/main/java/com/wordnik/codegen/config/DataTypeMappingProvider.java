package com.wordnik.codegen.config;

import java.util.List;

/**
 * Implementations of this class is responsible for generating mapping between rest data types and language
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
     * provide the sttring that needs to be used when defining methods that returns no values
     *
     * Example: in java this value will be <code>void</code>
     * @return
     */
    public String getReturnTypeForVoidMethods();

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
     * Gets list of imports that needs to be included when used objects of type List.
     *
     * Example: in java while using lists we use an interface of <Code>List</Code> and implementation of
     * <Code>ArrayList</Code>. SO the output will as follows:
     * <Code>
     *      List<String> imports = new ArrayList<String>();
            imports.add("java.util.List");
            imports.add("java.util.ArrayList");
     * </Code>
     * @return
     */
    public List<String> getListImportPackages();

    /**
     * Gets list of imports that needs to be included when used objects of type Map.
     *
     * Example: in java while using maps we use an interface of <Code>Map</Code> and implementation of
     * <Code>HashMap</Code>. SO the output will as follows:
     * <Code>
     *      List<String> imports = new ArrayList<String>();
            imports.add("java.util.Map");
            imports.add("java.util.HashMap");
     * </Code>
     * @return
     */
    public List<String> getMapImportPackages();

    /**
     * Gets list of imports that needs to be included when used objects of type Set.
     *
     * Example: in java while using sets we use an interface of <Code>Set</Code> and implementation of
     * <Code>HashSet</Code>. SO the output will as follows:
     * <Code>
     *      List<String> imports = new ArrayList<String>();
            imports.add("java.util.Set");
            imports.add("java.util.HashSet");
     * </Code>
     * @return
     */
    public List<String> getSetImportPackages();

    /**
     * Gets list of imports that needs to be included when used objects of type Date.
     *
     * Example: in java while using Data we use <Codejava.util.Date</Code>. So the output will as follows:
     * <Code>
     *      List<String> imports = new ArrayList<String>();
            imports.add("java.util.Date");
     * </Code>
     * @return
     */
    public List<String> getDateImports();

    /**
     * Object type definition for a given input
     *
     * @param type
     * @param primitiveObject
     * @return
     */
    public String getObjectType(String type, boolean primitiveObject);

    /**
     * Gets the value of return type converted from web service response documentation.
     *
     * Example: If the resource documentation ays return type as List[User] the equivalent translation for java will be
     *
     * <Code> List<User ></Code>
     *
     * If the input is Map[int, String] the equivalent java translation will be <Code> Map<Integer, String> </Code>
     * @param type
     * @return
     */
    public String getReturnValueType(String type);

    /**
     * Gets the class of return values from web service response documentation. If the service returns list the class
     * indicates type of object in the list
     *
     * Example: If the resource documentation ays return type as List[User] the equivalent translation for java will be
     *
     * <Code> User </Code>
     *
     * If the input is Map[int] the equivalent java translation will be <Code> Int </Code>
     * @param type
     * @return
     */
    public String getReturnClassType(String type);
}
