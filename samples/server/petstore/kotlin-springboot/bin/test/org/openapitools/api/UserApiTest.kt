package org.openapitools.api

import org.openapitools.model.User
import org.junit.jupiter.api.Test

import org.springframework.http.ResponseEntity

class UserApiTest {

    private val service: UserApiService = UserApiServiceImpl()
    private val api: UserApiController = UserApiController(service)

    
    /**
    * Create user
    *
    * This can only be done by the logged in user.
    *
    * @throws ApiException
    *          if the Api call fails
    */
    @Test
    fun createUserTest() {
        val body:User? = null
        val response: ResponseEntity<Unit> = api.createUser(body!!)

        // TODO: test validations
    }
    
    /**
    * Creates list of users with given input array
    *
    * 
    *
    * @throws ApiException
    *          if the Api call fails
    */
    @Test
    fun createUsersWithArrayInputTest() {
        val body:kotlin.collections.List<User>? = null
        val response: ResponseEntity<Unit> = api.createUsersWithArrayInput(body!!)

        // TODO: test validations
    }
    
    /**
    * Creates list of users with given input array
    *
    * 
    *
    * @throws ApiException
    *          if the Api call fails
    */
    @Test
    fun createUsersWithListInputTest() {
        val body:kotlin.collections.List<User>? = null
        val response: ResponseEntity<Unit> = api.createUsersWithListInput(body!!)

        // TODO: test validations
    }
    
    /**
    * Delete user
    *
    * This can only be done by the logged in user.
    *
    * @throws ApiException
    *          if the Api call fails
    */
    @Test
    fun deleteUserTest() {
        val username:kotlin.String? = null
        val response: ResponseEntity<Unit> = api.deleteUser(username!!)

        // TODO: test validations
    }
    
    /**
    * Get user by user name
    *
    * 
    *
    * @throws ApiException
    *          if the Api call fails
    */
    @Test
    fun getUserByNameTest() {
        val username:kotlin.String? = null
        val response: ResponseEntity<User> = api.getUserByName(username!!)

        // TODO: test validations
    }
    
    /**
    * Logs user into the system
    *
    * 
    *
    * @throws ApiException
    *          if the Api call fails
    */
    @Test
    fun loginUserTest() {
        val username:kotlin.String? = null
        val password:kotlin.String? = null
        val response: ResponseEntity<kotlin.String> = api.loginUser(username!!, password!!)

        // TODO: test validations
    }
    
    /**
    * Logs out current logged in user session
    *
    * 
    *
    * @throws ApiException
    *          if the Api call fails
    */
    @Test
    fun logoutUserTest() {
        val response: ResponseEntity<Unit> = api.logoutUser()

        // TODO: test validations
    }
    
    /**
    * Updated user
    *
    * This can only be done by the logged in user.
    *
    * @throws ApiException
    *          if the Api call fails
    */
    @Test
    fun updateUserTest() {
        val username:kotlin.String? = null
        val body:User? = null
        val response: ResponseEntity<Unit> = api.updateUser(username!!, body!!)

        // TODO: test validations
    }
    
}
