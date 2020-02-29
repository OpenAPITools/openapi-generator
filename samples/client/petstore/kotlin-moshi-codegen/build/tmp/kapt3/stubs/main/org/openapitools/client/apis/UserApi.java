package org.openapitools.client.apis;

import java.lang.System;

@kotlin.Metadata(mv = {1, 1, 16}, bv = {1, 0, 3}, k = 1, d1 = {"\u0000&\n\u0002\u0018\u0002\n\u0002\u0018\u0002\n\u0000\n\u0002\u0010\u000e\n\u0002\b\u0002\n\u0002\u0010\u0002\n\u0000\n\u0002\u0018\u0002\n\u0000\n\u0002\u0010\u0011\n\u0002\b\n\u0018\u00002\u00020\u0001B\u000f\u0012\b\b\u0002\u0010\u0002\u001a\u00020\u0003\u00a2\u0006\u0002\u0010\u0004J\u000e\u0010\u0005\u001a\u00020\u00062\u0006\u0010\u0007\u001a\u00020\bJ\u0019\u0010\t\u001a\u00020\u00062\f\u0010\u0007\u001a\b\u0012\u0004\u0012\u00020\b0\n\u00a2\u0006\u0002\u0010\u000bJ\u0019\u0010\f\u001a\u00020\u00062\f\u0010\u0007\u001a\b\u0012\u0004\u0012\u00020\b0\n\u00a2\u0006\u0002\u0010\u000bJ\u000e\u0010\r\u001a\u00020\u00062\u0006\u0010\u000e\u001a\u00020\u0003J\u000e\u0010\u000f\u001a\u00020\b2\u0006\u0010\u000e\u001a\u00020\u0003J\u0016\u0010\u0010\u001a\u00020\u00032\u0006\u0010\u000e\u001a\u00020\u00032\u0006\u0010\u0011\u001a\u00020\u0003J\u0006\u0010\u0012\u001a\u00020\u0006J\u0016\u0010\u0013\u001a\u00020\u00062\u0006\u0010\u000e\u001a\u00020\u00032\u0006\u0010\u0007\u001a\u00020\b\u00a8\u0006\u0014"}, d2 = {"Lorg/openapitools/client/apis/UserApi;", "Lorg/openapitools/client/infrastructure/ApiClient;", "basePath", "", "(Ljava/lang/String;)V", "createUser", "", "body", "Lorg/openapitools/client/models/User;", "createUsersWithArrayInput", "", "([Lorg/openapitools/client/models/User;)V", "createUsersWithListInput", "deleteUser", "username", "getUserByName", "loginUser", "password", "logoutUser", "updateUser", "kotlin-petstore-moshi-codegen"})
public final class UserApi extends org.openapitools.client.infrastructure.ApiClient {
    
    /**
     * Create user
     * This can only be done by the logged in user.
     * @param body Created user object 
     * @return void
     * @throws UnsupportedOperationException If the API returns an informational or redirection response
     * @throws ClientException If the API returns a client error response
     * @throws ServerException If the API returns a server error response
     */
    public final void createUser(@org.jetbrains.annotations.NotNull()
    org.openapitools.client.models.User body) throws java.lang.UnsupportedOperationException, org.openapitools.client.infrastructure.ClientException, org.openapitools.client.infrastructure.ServerException {
    }
    
    /**
     * Creates list of users with given input array
     *
     * @param body List of user object 
     * @return void
     * @throws UnsupportedOperationException If the API returns an informational or redirection response
     * @throws ClientException If the API returns a client error response
     * @throws ServerException If the API returns a server error response
     */
    public final void createUsersWithArrayInput(@org.jetbrains.annotations.NotNull()
    org.openapitools.client.models.User[] body) throws java.lang.UnsupportedOperationException, org.openapitools.client.infrastructure.ClientException, org.openapitools.client.infrastructure.ServerException {
    }
    
    /**
     * Creates list of users with given input array
     *
     * @param body List of user object 
     * @return void
     * @throws UnsupportedOperationException If the API returns an informational or redirection response
     * @throws ClientException If the API returns a client error response
     * @throws ServerException If the API returns a server error response
     */
    public final void createUsersWithListInput(@org.jetbrains.annotations.NotNull()
    org.openapitools.client.models.User[] body) throws java.lang.UnsupportedOperationException, org.openapitools.client.infrastructure.ClientException, org.openapitools.client.infrastructure.ServerException {
    }
    
    /**
     * Delete user
     * This can only be done by the logged in user.
     * @param username The name that needs to be deleted 
     * @return void
     * @throws UnsupportedOperationException If the API returns an informational or redirection response
     * @throws ClientException If the API returns a client error response
     * @throws ServerException If the API returns a server error response
     */
    public final void deleteUser(@org.jetbrains.annotations.NotNull()
    java.lang.String username) throws java.lang.UnsupportedOperationException, org.openapitools.client.infrastructure.ClientException, org.openapitools.client.infrastructure.ServerException {
    }
    
    /**
     * Get user by user name
     *
     * @param username The name that needs to be fetched. Use user1 for testing. 
     * @return User
     * @throws UnsupportedOperationException If the API returns an informational or redirection response
     * @throws ClientException If the API returns a client error response
     * @throws ServerException If the API returns a server error response
     */
    @org.jetbrains.annotations.NotNull()
    @kotlin.Suppress(names = {"UNCHECKED_CAST"})
    public final org.openapitools.client.models.User getUserByName(@org.jetbrains.annotations.NotNull()
    java.lang.String username) throws java.lang.UnsupportedOperationException, org.openapitools.client.infrastructure.ClientException, org.openapitools.client.infrastructure.ServerException {
        return null;
    }
    
    /**
     * Logs user into the system
     *
     * @param username The user name for login 
     * @param password The password for login in clear text 
     * @return kotlin.String
     * @throws UnsupportedOperationException If the API returns an informational or redirection response
     * @throws ClientException If the API returns a client error response
     * @throws ServerException If the API returns a server error response
     */
    @org.jetbrains.annotations.NotNull()
    @kotlin.Suppress(names = {"UNCHECKED_CAST"})
    public final java.lang.String loginUser(@org.jetbrains.annotations.NotNull()
    java.lang.String username, @org.jetbrains.annotations.NotNull()
    java.lang.String password) throws java.lang.UnsupportedOperationException, org.openapitools.client.infrastructure.ClientException, org.openapitools.client.infrastructure.ServerException {
        return null;
    }
    
    /**
     * Logs out current logged in user session
     *
     * @return void
     * @throws UnsupportedOperationException If the API returns an informational or redirection response
     * @throws ClientException If the API returns a client error response
     * @throws ServerException If the API returns a server error response
     */
    public final void logoutUser() throws java.lang.UnsupportedOperationException, org.openapitools.client.infrastructure.ClientException, org.openapitools.client.infrastructure.ServerException {
    }
    
    /**
     * Updated user
     * This can only be done by the logged in user.
     * @param username name that need to be deleted 
     * @param body Updated user object 
     * @return void
     * @throws UnsupportedOperationException If the API returns an informational or redirection response
     * @throws ClientException If the API returns a client error response
     * @throws ServerException If the API returns a server error response
     */
    public final void updateUser(@org.jetbrains.annotations.NotNull()
    java.lang.String username, @org.jetbrains.annotations.NotNull()
    org.openapitools.client.models.User body) throws java.lang.UnsupportedOperationException, org.openapitools.client.infrastructure.ClientException, org.openapitools.client.infrastructure.ServerException {
    }
    
    public UserApi(@org.jetbrains.annotations.NotNull()
    java.lang.String basePath) {
        super(null);
    }
    
    public UserApi() {
        super(null);
    }
}