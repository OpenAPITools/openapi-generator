/*
 * OpenAPI Petstore
 *
 * This is a sample server Petstore server. For this sample, you can use the api key `special-key` to test the authorization filters.
 *
 * OpenAPI document version: 1.0.0
 * 
 *
 * AUTO-GENERATED FILE, DO NOT MODIFY!
 */
package org.openapitools.handler;

import io.undertow.server.*;
import io.undertow.util.*;

import org.openapitools.model.*;

@SuppressWarnings("TooManyFunctions")
public interface PathHandlerInterface {

    /**
     * <p>Add a new pet to the store</p>
     *
     * <p><b>Endpoint</b>: {@link Methods#POST POST} "/v2/pet" (<i>privileged: true</i>)</p>
     *
     * <p><b>Request parameters</b>:</p>
     * <ul>
     * </ul>
     *
     * <p><b>Consumes</b>: [{mediaType=application/json}, {mediaType=application/xml}]</p>
     * <p><b>Payload</b>: {@link Pet} (<i>required: true</i>)</p>
     *
     *
     * <p><b>Responses</b>:</p>
     * <ul>
     * <li><b>405 (client error)</b>: Invalid input</li>
     * </ul>
     */
    @javax.annotation.Nonnull
    HttpHandler addPet();

    /**
     * <p>Deletes a pet</p>
     *
     * <p><b>Endpoint</b>: {@link Methods#DELETE DELETE} "/v2/pet/{petId}" (<i>privileged: true</i>)</p>
     *
     * <p><b>Request parameters</b>:</p>
     * <ul>
     * <li>
     * <p>"<b>petId</b>"
     * <p>Pet id to delete</p>
     * <p>
     * - Parameter type: <b>{@link Long}</b><br/>
     * - Appears in: <b>{@link HttpServerExchange#getPathParameters Path}</b><br/>
     * - Required: <b>true</b>
     * </p>
     * </li>
     * <li>
     * <p>"<b>api_key</b>"
     * <p>
     * - Parameter type: <b>{@link String}</b><br/>
     * - Appears in: <b>{@link Headers Header}</b><br/>
     * - Required: <b>false</b>
     * </p>
     * </li>
     * </ul>
     *
     *
     * <p><b>Responses</b>:</p>
     * <ul>
     * <li><b>400 (client error)</b>: Invalid pet value</li>
     * </ul>
     */
    @javax.annotation.Nonnull
    HttpHandler deletePet();

    /**
     * <p>Finds Pets by status</p>
     *
     * <p>Multiple status values can be provided with comma separated strings</p>
     *
     * <p><b>Endpoint</b>: {@link Methods#GET GET} "/v2/pet/findByStatus" (<i>privileged: true</i>)</p>
     *
     * <p><b>Request parameters</b>:</p>
     * <ul>
     * <li>
     * <p>"<b>status</b>"
     * <p>Status values that need to be considered for filter</p>
     * <p>
     * - Parameter type: <b>{@link java.util.List List} of {@link List&lt;String&gt;}</b><br/>
     * - Appears in: <b>{@link HttpServerExchange#getQueryParameters Query}</b><br/>
     * - Default value: <b>new ArrayList<String>()</b><br/>
     * - Required: <b>true</b>
     * </p>
     * </li>
     * </ul>
     *
     * <p><b>Produces</b>: [{mediaType=application/xml}, {mediaType=application/json}]</p>
     * <p><b>Returns</b>: {@link java.util.List List} of {@link Pet}</p>
     *
     * <p><b>Responses</b>:</p>
     * <ul>
     * <li><b>200 (success)</b>: successful operation</li>
     * <li><b>400 (client error)</b>: Invalid status value</li>
     * </ul>
     */
    @javax.annotation.Nonnull
    HttpHandler findPetsByStatus();

    /**
     * <p>Finds Pets by tags</p>
     *
     * <p>Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.</p>
     *
     * <p><b>Endpoint</b>: {@link Methods#GET GET} "/v2/pet/findByTags" (<i>privileged: true</i>)</p>
     *
     * <p><b>Request parameters</b>:</p>
     * <ul>
     * <li>
     * <p>"<b>tags</b>"
     * <p>Tags to filter by</p>
     * <p>
     * - Parameter type: <b>{@link java.util.List List} of {@link List&lt;String&gt;}</b><br/>
     * - Appears in: <b>{@link HttpServerExchange#getQueryParameters Query}</b><br/>
     * - Default value: <b>new ArrayList<String>()</b><br/>
     * - Required: <b>true</b>
     * </p>
     * </li>
     * </ul>
     *
     * <p><b>Produces</b>: [{mediaType=application/xml}, {mediaType=application/json}]</p>
     * <p><b>Returns</b>: {@link java.util.List List} of {@link Pet}</p>
     *
     * <p><b>Responses</b>:</p>
     * <ul>
     * <li><b>200 (success)</b>: successful operation</li>
     * <li><b>400 (client error)</b>: Invalid tag value</li>
     * </ul>
     */
    @javax.annotation.Nonnull
    @Deprecated
    HttpHandler findPetsByTags();

    /**
     * <p>Find pet by ID</p>
     *
     * <p>Returns a single pet</p>
     *
     * <p><b>Endpoint</b>: {@link Methods#GET GET} "/v2/pet/{petId}" (<i>privileged: true</i>)</p>
     *
     * <p><b>Request parameters</b>:</p>
     * <ul>
     * <li>
     * <p>"<b>petId</b>"
     * <p>ID of pet to return</p>
     * <p>
     * - Parameter type: <b>{@link Long}</b><br/>
     * - Appears in: <b>{@link HttpServerExchange#getPathParameters Path}</b><br/>
     * - Required: <b>true</b>
     * </p>
     * </li>
     * </ul>
     *
     * <p><b>Produces</b>: [{mediaType=application/xml}, {mediaType=application/json}]</p>
     * <p><b>Returns</b>: {@link Pet}</p>
     *
     * <p><b>Responses</b>:</p>
     * <ul>
     * <li><b>200 (success)</b>: successful operation</li>
     * <li><b>400 (client error)</b>: Invalid ID supplied</li>
     * <li><b>404 (client error)</b>: Pet not found</li>
     * </ul>
     */
    @javax.annotation.Nonnull
    HttpHandler getPetById();

    /**
     * <p>Update an existing pet</p>
     *
     * <p><b>Endpoint</b>: {@link Methods#PUT PUT} "/v2/pet" (<i>privileged: true</i>)</p>
     *
     * <p><b>Request parameters</b>:</p>
     * <ul>
     * </ul>
     *
     * <p><b>Consumes</b>: [{mediaType=application/json}, {mediaType=application/xml}]</p>
     * <p><b>Payload</b>: {@link Pet} (<i>required: true</i>)</p>
     *
     *
     * <p><b>Responses</b>:</p>
     * <ul>
     * <li><b>400 (client error)</b>: Invalid ID supplied</li>
     * <li><b>404 (client error)</b>: Pet not found</li>
     * <li><b>405 (client error)</b>: Validation exception</li>
     * </ul>
     */
    @javax.annotation.Nonnull
    HttpHandler updatePet();

    /**
     * <p>Updates a pet in the store with form data</p>
     *
     * <p><b>Endpoint</b>: {@link Methods#POST POST} "/v2/pet/{petId}" (<i>privileged: true</i>)</p>
     *
     * <p><b>Request parameters</b>:</p>
     * <ul>
     * <li>
     * <p>"<b>petId</b>"
     * <p>ID of pet that needs to be updated</p>
     * <p>
     * - Parameter type: <b>{@link Long}</b><br/>
     * - Appears in: <b>{@link HttpServerExchange#getPathParameters Path}</b><br/>
     * - Required: <b>true</b>
     * </p>
     * </li>
     * <li>
     * <p>"<b>name</b>"
     * <p>Updated name of the pet</p>
     * <p>
     * - Parameter type: <b>{@link String}</b><br/>
     * - Appears in: <b>{@link io.undertow.server.handlers.form.FormDataParser Form}</b><br/>
     * - Required: <b>false</b>
     * </p>
     * </li>
     * <li>
     * <p>"<b>status</b>"
     * <p>Updated status of the pet</p>
     * <p>
     * - Parameter type: <b>{@link String}</b><br/>
     * - Appears in: <b>{@link io.undertow.server.handlers.form.FormDataParser Form}</b><br/>
     * - Required: <b>false</b>
     * </p>
     * </li>
     * </ul>
     *
     * <p><b>Consumes</b>: [{mediaType=application/x-www-form-urlencoded}]</p>
     *
     *
     * <p><b>Responses</b>:</p>
     * <ul>
     * <li><b>405 (client error)</b>: Invalid input</li>
     * </ul>
     */
    @javax.annotation.Nonnull
    HttpHandler updatePetWithForm();

    /**
     * <p>uploads an image</p>
     *
     * <p><b>Endpoint</b>: {@link Methods#POST POST} "/v2/pet/{petId}/uploadImage" (<i>privileged: true</i>)</p>
     *
     * <p><b>Request parameters</b>:</p>
     * <ul>
     * <li>
     * <p>"<b>petId</b>"
     * <p>ID of pet to update</p>
     * <p>
     * - Parameter type: <b>{@link Long}</b><br/>
     * - Appears in: <b>{@link HttpServerExchange#getPathParameters Path}</b><br/>
     * - Required: <b>true</b>
     * </p>
     * </li>
     * <li>
     * <p>"<b>additionalMetadata</b>"
     * <p>Additional data to pass to server</p>
     * <p>
     * - Parameter type: <b>{@link String}</b><br/>
     * - Appears in: <b>{@link io.undertow.server.handlers.form.FormDataParser Form}</b><br/>
     * - Required: <b>false</b>
     * </p>
     * </li>
     * <li>
     * <p>"<b>file</b>"
     * <p>file to upload</p>
     * <p>
     * - Parameter type: <b>BinaryFile</b><br/>
     * - Appears in: <b>{@link io.undertow.server.handlers.form.FormDataParser Form}</b><br/>
     * - Required: <b>false</b>
     * </p>
     * </li>
     * </ul>
     *
     * <p><b>Consumes</b>: [{mediaType=multipart/form-data}]</p>
     *
     * <p><b>Produces</b>: [{mediaType=application/json}]</p>
     * <p><b>Returns</b>: {@link ModelApiResponse}</p>
     *
     * <p><b>Responses</b>:</p>
     * <ul>
     * <li><b>200 (success)</b>: successful operation</li>
     * </ul>
     */
    @javax.annotation.Nonnull
    HttpHandler uploadFile();

    /**
     * <p>Delete purchase order by ID</p>
     *
     * <p>For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors</p>
     *
     * <p><b>Endpoint</b>: {@link Methods#DELETE DELETE} "/v2/store/order/{orderId}" (<i>privileged: false</i>)</p>
     *
     * <p><b>Request parameters</b>:</p>
     * <ul>
     * <li>
     * <p>"<b>orderId</b>"
     * <p>ID of the order that needs to be deleted</p>
     * <p>
     * - Parameter type: <b>{@link String}</b><br/>
     * - Appears in: <b>{@link HttpServerExchange#getPathParameters Path}</b><br/>
     * - Required: <b>true</b>
     * </p>
     * </li>
     * </ul>
     *
     *
     * <p><b>Responses</b>:</p>
     * <ul>
     * <li><b>400 (client error)</b>: Invalid ID supplied</li>
     * <li><b>404 (client error)</b>: Order not found</li>
     * </ul>
     */
    @javax.annotation.Nonnull
    HttpHandler deleteOrder();

    /**
     * <p>Returns pet inventories by status</p>
     *
     * <p>Returns a map of status codes to quantities</p>
     *
     * <p><b>Endpoint</b>: {@link Methods#GET GET} "/v2/store/inventory" (<i>privileged: true</i>)</p>
     *
     * <p><b>Produces</b>: [{mediaType=application/json}]</p>
     * <p><b>Returns</b>: {@link java.util.Map Map} of {@link Integer}</p>
     *
     * <p><b>Responses</b>:</p>
     * <ul>
     * <li><b>200 (success)</b>: successful operation</li>
     * </ul>
     */
    @javax.annotation.Nonnull
    HttpHandler getInventory();

    /**
     * <p>Find purchase order by ID</p>
     *
     * <p>For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions</p>
     *
     * <p><b>Endpoint</b>: {@link Methods#GET GET} "/v2/store/order/{orderId}" (<i>privileged: false</i>)</p>
     *
     * <p><b>Request parameters</b>:</p>
     * <ul>
     * <li>
     * <p>"<b>orderId</b>"
     * <p>ID of pet that needs to be fetched</p>
     * <p>
     * - Parameter type: <b>{@link Long}</b><br/>
     * - Appears in: <b>{@link HttpServerExchange#getPathParameters Path}</b><br/>
     * - Required: <b>true</b>
     * </p>
     * </li>
     * </ul>
     *
     * <p><b>Produces</b>: [{mediaType=application/xml}, {mediaType=application/json}]</p>
     * <p><b>Returns</b>: {@link Order}</p>
     *
     * <p><b>Responses</b>:</p>
     * <ul>
     * <li><b>200 (success)</b>: successful operation</li>
     * <li><b>400 (client error)</b>: Invalid ID supplied</li>
     * <li><b>404 (client error)</b>: Order not found</li>
     * </ul>
     */
    @javax.annotation.Nonnull
    HttpHandler getOrderById();

    /**
     * <p>Place an order for a pet</p>
     *
     * <p><b>Endpoint</b>: {@link Methods#POST POST} "/v2/store/order" (<i>privileged: false</i>)</p>
     *
     * <p><b>Request parameters</b>:</p>
     * <ul>
     * </ul>
     *
     * <p><b>Produces</b>: [{mediaType=application/xml}, {mediaType=application/json}]</p>
     * <p><b>Returns</b>: {@link Order}</p>
     *
     * <p><b>Responses</b>:</p>
     * <ul>
     * <li><b>200 (success)</b>: successful operation</li>
     * <li><b>400 (client error)</b>: Invalid Order</li>
     * </ul>
     */
    @javax.annotation.Nonnull
    HttpHandler placeOrder();

    /**
     * <p>Create user</p>
     *
     * <p>This can only be done by the logged in user.</p>
     *
     * <p><b>Endpoint</b>: {@link Methods#POST POST} "/v2/user" (<i>privileged: false</i>)</p>
     *
     * <p><b>Request parameters</b>:</p>
     * <ul>
     * </ul>
     *
     *
     * <p><b>Responses</b>:</p>
     * <ul>
     * <li><b>Default</b>: successful operation</li>
     * </ul>
     */
    @javax.annotation.Nonnull
    HttpHandler createUser();

    /**
     * <p>Creates list of users with given input array</p>
     *
     * <p><b>Endpoint</b>: {@link Methods#POST POST} "/v2/user/createWithArray" (<i>privileged: false</i>)</p>
     *
     * <p><b>Request parameters</b>:</p>
     * <ul>
     * </ul>
     *
     *
     * <p><b>Responses</b>:</p>
     * <ul>
     * <li><b>Default</b>: successful operation</li>
     * </ul>
     */
    @javax.annotation.Nonnull
    HttpHandler createUsersWithArrayInput();

    /**
     * <p>Creates list of users with given input array</p>
     *
     * <p><b>Endpoint</b>: {@link Methods#POST POST} "/v2/user/createWithList" (<i>privileged: false</i>)</p>
     *
     * <p><b>Request parameters</b>:</p>
     * <ul>
     * </ul>
     *
     *
     * <p><b>Responses</b>:</p>
     * <ul>
     * <li><b>Default</b>: successful operation</li>
     * </ul>
     */
    @javax.annotation.Nonnull
    HttpHandler createUsersWithListInput();

    /**
     * <p>Delete user</p>
     *
     * <p>This can only be done by the logged in user.</p>
     *
     * <p><b>Endpoint</b>: {@link Methods#DELETE DELETE} "/v2/user/{username}" (<i>privileged: false</i>)</p>
     *
     * <p><b>Request parameters</b>:</p>
     * <ul>
     * <li>
     * <p>"<b>username</b>"
     * <p>The name that needs to be deleted</p>
     * <p>
     * - Parameter type: <b>{@link String}</b><br/>
     * - Appears in: <b>{@link HttpServerExchange#getPathParameters Path}</b><br/>
     * - Required: <b>true</b>
     * </p>
     * </li>
     * </ul>
     *
     *
     * <p><b>Responses</b>:</p>
     * <ul>
     * <li><b>400 (client error)</b>: Invalid username supplied</li>
     * <li><b>404 (client error)</b>: User not found</li>
     * </ul>
     */
    @javax.annotation.Nonnull
    HttpHandler deleteUser();

    /**
     * <p>Get user by user name</p>
     *
     * <p><b>Endpoint</b>: {@link Methods#GET GET} "/v2/user/{username}" (<i>privileged: false</i>)</p>
     *
     * <p><b>Request parameters</b>:</p>
     * <ul>
     * <li>
     * <p>"<b>username</b>"
     * <p>The name that needs to be fetched. Use user1 for testing.</p>
     * <p>
     * - Parameter type: <b>{@link String}</b><br/>
     * - Appears in: <b>{@link HttpServerExchange#getPathParameters Path}</b><br/>
     * - Required: <b>true</b>
     * </p>
     * </li>
     * </ul>
     *
     * <p><b>Produces</b>: [{mediaType=application/xml}, {mediaType=application/json}]</p>
     * <p><b>Returns</b>: {@link User}</p>
     *
     * <p><b>Responses</b>:</p>
     * <ul>
     * <li><b>200 (success)</b>: successful operation</li>
     * <li><b>400 (client error)</b>: Invalid username supplied</li>
     * <li><b>404 (client error)</b>: User not found</li>
     * </ul>
     */
    @javax.annotation.Nonnull
    HttpHandler getUserByName();

    /**
     * <p>Logs user into the system</p>
     *
     * <p><b>Endpoint</b>: {@link Methods#GET GET} "/v2/user/login" (<i>privileged: false</i>)</p>
     *
     * <p><b>Request parameters</b>:</p>
     * <ul>
     * <li>
     * <p>"<b>username</b>"
     * <p>The user name for login</p>
     * <p>
     * - Parameter type: <b>{@link String}</b><br/>
     * - Appears in: <b>{@link HttpServerExchange#getQueryParameters Query}</b><br/>
     * - Required: <b>true</b>
     * </p>
     * </li>
     * <li>
     * <p>"<b>password</b>"
     * <p>The password for login in clear text</p>
     * <p>
     * - Parameter type: <b>{@link String}</b><br/>
     * - Appears in: <b>{@link HttpServerExchange#getQueryParameters Query}</b><br/>
     * - Required: <b>true</b>
     * </p>
     * </li>
     * </ul>
     * <p><b>Response headers</b>: [CodegenProperty{openApiType&#x3D;&#39;integer&#39;, baseName&#x3D;&#39;X-Rate-Limit&#39;, complexType&#x3D;&#39;null&#39;, getter&#x3D;&#39;getxRateLimit&#39;, setter&#x3D;&#39;setxRateLimit&#39;, description&#x3D;&#39;calls per hour allowed by the user&#39;, dataType&#x3D;&#39;Integer&#39;, datatypeWithEnum&#x3D;&#39;Integer&#39;, dataFormat&#x3D;&#39;int32&#39;, name&#x3D;&#39;xRateLimit&#39;, min&#x3D;&#39;null&#39;, max&#x3D;&#39;null&#39;, defaultValue&#x3D;&#39;null&#39;, defaultValueWithParam&#x3D;&#39; &#x3D; data.X-Rate-Limit;&#39;, baseType&#x3D;&#39;Integer&#39;, containerType&#x3D;&#39;null&#39;, title&#x3D;&#39;null&#39;, unescapedDescription&#x3D;&#39;calls per hour allowed by the user&#39;, maxLength&#x3D;null, minLength&#x3D;null, pattern&#x3D;&#39;null&#39;, example&#x3D;&#39;null&#39;, jsonSchema&#x3D;&#39;{
  &quot;type&quot; : &quot;integer&quot;,
  &quot;format&quot; : &quot;int32&quot;
}&#39;, minimum&#x3D;&#39;null&#39;, maximum&#x3D;&#39;null&#39;, exclusiveMinimum&#x3D;false, exclusiveMaximum&#x3D;false, required&#x3D;false, deprecated&#x3D;false, hasMoreNonReadOnly&#x3D;false, isPrimitiveType&#x3D;true, isModel&#x3D;false, isContainer&#x3D;false, isString&#x3D;false, isNumeric&#x3D;true, isInteger&#x3D;true, isLong&#x3D;false, isNumber&#x3D;false, isFloat&#x3D;false, isDouble&#x3D;false, isDecimal&#x3D;false, isByteArray&#x3D;false, isBinary&#x3D;false, isFile&#x3D;false, isBoolean&#x3D;false, isDate&#x3D;false, isDateTime&#x3D;false, isUuid&#x3D;false, isUri&#x3D;false, isEmail&#x3D;false, isFreeFormObject&#x3D;false, isArray&#x3D;false, isMap&#x3D;false, isEnum&#x3D;false, isReadOnly&#x3D;false, isWriteOnly&#x3D;false, isNullable&#x3D;false, isSelfReference&#x3D;false, isCircularReference&#x3D;false, isDiscriminator&#x3D;false, _enum&#x3D;null, allowableValues&#x3D;null, items&#x3D;null, additionalProperties&#x3D;null, vars&#x3D;[], requiredVars&#x3D;[], mostInnerItems&#x3D;null, vendorExtensions&#x3D;{}, hasValidation&#x3D;false, isInherited&#x3D;false, discriminatorValue&#x3D;&#39;null&#39;, nameInCamelCase&#x3D;&#39;XRateLimit&#39;, nameInSnakeCase&#x3D;&#39;X_RATE_LIMIT&#39;, enumName&#x3D;&#39;null&#39;, maxItems&#x3D;null, minItems&#x3D;null, maxProperties&#x3D;null, minProperties&#x3D;null, uniqueItems&#x3D;false, multipleOf&#x3D;null, isXmlAttribute&#x3D;false, xmlPrefix&#x3D;&#39;null&#39;, xmlName&#x3D;&#39;null&#39;, xmlNamespace&#x3D;&#39;null&#39;, isXmlWrapped&#x3D;false}, CodegenProperty{openApiType&#x3D;&#39;string&#39;, baseName&#x3D;&#39;X-Expires-After&#39;, complexType&#x3D;&#39;Date&#39;, getter&#x3D;&#39;getxExpiresAfter&#39;, setter&#x3D;&#39;setxExpiresAfter&#39;, description&#x3D;&#39;date in UTC when toekn expires&#39;, dataType&#x3D;&#39;Date&#39;, datatypeWithEnum&#x3D;&#39;Date&#39;, dataFormat&#x3D;&#39;date-time&#39;, name&#x3D;&#39;xExpiresAfter&#39;, min&#x3D;&#39;null&#39;, max&#x3D;&#39;null&#39;, defaultValue&#x3D;&#39;null&#39;, defaultValueWithParam&#x3D;&#39; &#x3D; data.X-Expires-After;&#39;, baseType&#x3D;&#39;Date&#39;, containerType&#x3D;&#39;null&#39;, title&#x3D;&#39;null&#39;, unescapedDescription&#x3D;&#39;date in UTC when toekn expires&#39;, maxLength&#x3D;null, minLength&#x3D;null, pattern&#x3D;&#39;null&#39;, example&#x3D;&#39;null&#39;, jsonSchema&#x3D;&#39;{
  &quot;type&quot; : &quot;string&quot;,
  &quot;format&quot; : &quot;date-time&quot;
}&#39;, minimum&#x3D;&#39;null&#39;, maximum&#x3D;&#39;null&#39;, exclusiveMinimum&#x3D;false, exclusiveMaximum&#x3D;false, required&#x3D;false, deprecated&#x3D;false, hasMoreNonReadOnly&#x3D;false, isPrimitiveType&#x3D;false, isModel&#x3D;false, isContainer&#x3D;false, isString&#x3D;false, isNumeric&#x3D;false, isInteger&#x3D;false, isLong&#x3D;false, isNumber&#x3D;false, isFloat&#x3D;false, isDouble&#x3D;false, isDecimal&#x3D;false, isByteArray&#x3D;false, isBinary&#x3D;false, isFile&#x3D;false, isBoolean&#x3D;false, isDate&#x3D;false, isDateTime&#x3D;true, isUuid&#x3D;false, isUri&#x3D;false, isEmail&#x3D;false, isFreeFormObject&#x3D;false, isArray&#x3D;false, isMap&#x3D;false, isEnum&#x3D;false, isReadOnly&#x3D;false, isWriteOnly&#x3D;false, isNullable&#x3D;false, isSelfReference&#x3D;false, isCircularReference&#x3D;false, isDiscriminator&#x3D;false, _enum&#x3D;null, allowableValues&#x3D;null, items&#x3D;null, additionalProperties&#x3D;null, vars&#x3D;[], requiredVars&#x3D;[], mostInnerItems&#x3D;null, vendorExtensions&#x3D;{}, hasValidation&#x3D;false, isInherited&#x3D;false, discriminatorValue&#x3D;&#39;null&#39;, nameInCamelCase&#x3D;&#39;XExpiresAfter&#39;, nameInSnakeCase&#x3D;&#39;X_EXPIRES_AFTER&#39;, enumName&#x3D;&#39;null&#39;, maxItems&#x3D;null, minItems&#x3D;null, maxProperties&#x3D;null, minProperties&#x3D;null, uniqueItems&#x3D;false, multipleOf&#x3D;null, isXmlAttribute&#x3D;false, xmlPrefix&#x3D;&#39;null&#39;, xmlName&#x3D;&#39;null&#39;, xmlNamespace&#x3D;&#39;null&#39;, isXmlWrapped&#x3D;false}]</p>
     *
     * <p><b>Produces</b>: [{mediaType=application/xml}, {mediaType=application/json}]</p>
     * <p><b>Returns</b>: {@link String}</p>
     *
     * <p><b>Responses</b>:</p>
     * <ul>
     * <li><b>200 (success)</b>: successful operation</li>
     * <li><b>400 (client error)</b>: Invalid username/password supplied</li>
     * </ul>
     */
    @javax.annotation.Nonnull
    HttpHandler loginUser();

    /**
     * <p>Logs out current logged in user session</p>
     *
     * <p><b>Endpoint</b>: {@link Methods#GET GET} "/v2/user/logout" (<i>privileged: false</i>)</p>
     *
     *
     * <p><b>Responses</b>:</p>
     * <ul>
     * <li><b>Default</b>: successful operation</li>
     * </ul>
     */
    @javax.annotation.Nonnull
    HttpHandler logoutUser();

    /**
     * <p>Updated user</p>
     *
     * <p>This can only be done by the logged in user.</p>
     *
     * <p><b>Endpoint</b>: {@link Methods#PUT PUT} "/v2/user/{username}" (<i>privileged: false</i>)</p>
     *
     * <p><b>Request parameters</b>:</p>
     * <ul>
     * <li>
     * <p>"<b>username</b>"
     * <p>name that need to be deleted</p>
     * <p>
     * - Parameter type: <b>{@link String}</b><br/>
     * - Appears in: <b>{@link HttpServerExchange#getPathParameters Path}</b><br/>
     * - Required: <b>true</b>
     * </p>
     * </li>
     * </ul>
     *
     *
     * <p><b>Responses</b>:</p>
     * <ul>
     * <li><b>400 (client error)</b>: Invalid user supplied</li>
     * <li><b>404 (client error)</b>: User not found</li>
     * </ul>
     */
    @javax.annotation.Nonnull
    HttpHandler updateUser();
}
