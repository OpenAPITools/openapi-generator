package org.openapitools.server.apis

import org.openapitools.server.models.User
import io.javalin.http.Context

interface UserApiService {

    /**
     * POST /user : Create user
     * This can only be done by the logged in user.
     *
     * @param user Created user object (required)
     * @param ctx The Javalin context. Especially handy if you need to access things like authentication headers in your service. (required)
     * @return successful operation (status code 200)
     * @see UserApi#createUser
     */
    fun createUser(user: User, ctx: Context): Unit

    /**
     * POST /user/createWithArray : Creates list of users with given input array
     * 
     *
     * @param user List of user object (required)
     * @param ctx The Javalin context. Especially handy if you need to access things like authentication headers in your service. (required)
     * @return successful operation (status code 200)
     * @see UserApi#createUsersWithArrayInput
     */
    fun createUsersWithArrayInput(user: kotlin.collections.List<User>, ctx: Context): Unit

    /**
     * POST /user/createWithList : Creates list of users with given input array
     * 
     *
     * @param user List of user object (required)
     * @param ctx The Javalin context. Especially handy if you need to access things like authentication headers in your service. (required)
     * @return successful operation (status code 200)
     * @see UserApi#createUsersWithListInput
     */
    fun createUsersWithListInput(user: kotlin.collections.List<User>, ctx: Context): Unit

    /**
     * DELETE /user/{username} : Delete user
     * This can only be done by the logged in user.
     *
     * @param username The name that needs to be deleted (required)
     * @param ctx The Javalin context. Especially handy if you need to access things like authentication headers in your service. (required)
     * @return Invalid username supplied (status code 400)
     *         or User not found (status code 404)
     * @see UserApi#deleteUser
     */
    fun deleteUser(username: kotlin.String, ctx: Context): Unit

    /**
     * GET /user/{username} : Get user by user name
     * 
     *
     * @param username The name that needs to be fetched. Use user1 for testing. (required)
     * @param ctx The Javalin context. Especially handy if you need to access things like authentication headers in your service. (required)
     * @return successful operation (status code 200)
     *         or Invalid username supplied (status code 400)
     *         or User not found (status code 404)
     * @see UserApi#getUserByName
     */
    fun getUserByName(username: kotlin.String, ctx: Context): User

    /**
     * GET /user/login : Logs user into the system
     * 
     *
     * @param username The user name for login (required)
     * @param password The password for login in clear text (required)
     * @param ctx The Javalin context. Especially handy if you need to access things like authentication headers in your service. (required)
     * @return successful operation (status code 200)
     *         or Invalid username/password supplied (status code 400)
     * @see UserApi#loginUser
     */
    fun loginUser(username: kotlin.String, password: kotlin.String, ctx: Context): kotlin.String

    /**
     * GET /user/logout : Logs out current logged in user session
     * 
     *
     * @param ctx The Javalin context. Especially handy if you need to access things like authentication headers in your service. (required)
     * @return successful operation (status code 200)
     * @see UserApi#logoutUser
     */
    fun logoutUser(ctx: Context): Unit

    /**
     * PUT /user/{username} : Updated user
     * This can only be done by the logged in user.
     *
     * @param username name that need to be deleted (required)
     * @param user Updated user object (required)
     * @param ctx The Javalin context. Especially handy if you need to access things like authentication headers in your service. (required)
     * @return Invalid user supplied (status code 400)
     *         or User not found (status code 404)
     * @see UserApi#updateUser
     */
    fun updateUser(username: kotlin.String, user: User, ctx: Context): Unit
}
