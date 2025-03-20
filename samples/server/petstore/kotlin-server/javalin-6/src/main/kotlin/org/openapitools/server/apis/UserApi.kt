package org.openapitools.server.apis

import io.javalin.http.Context
import io.javalin.http.bodyAsClass
import io.javalin.http.pathParamAsClass
import io.javalin.http.queryParamAsClass

import org.openapitools.server.models.User

class UserApi(private val service: UserApiService) {
    /**
     * Create user
     * This can only be done by the logged in user.
     * @param user Created user object 
     */
    fun createUser(ctx: Context) {
        val result = service.createUser(ctx.bodyAsClass<User>(), ctx)
        ctx.status(200).json(result)
    }

    /**
     * Creates list of users with given input array
     * 
     * @param user List of user object 
     */
    fun createUsersWithArrayInput(ctx: Context) {
        val result = service.createUsersWithArrayInput(ctx.bodyAsClass<List<User>>(), ctx)
        ctx.status(200).json(result)
    }

    /**
     * Creates list of users with given input array
     * 
     * @param user List of user object 
     */
    fun createUsersWithListInput(ctx: Context) {
        val result = service.createUsersWithListInput(ctx.bodyAsClass<List<User>>(), ctx)
        ctx.status(200).json(result)
    }

    /**
     * Delete user
     * This can only be done by the logged in user.
     * @param username The name that needs to be deleted 
     */
    fun deleteUser(ctx: Context) {
        val result = service.deleteUser(ctx.pathParamAsClass<kotlin.String>("username").get(), ctx)
        ctx.status(400).json(result)
    }

    /**
     * Get user by user name
     * 
     * @param username The name that needs to be fetched. Use user1 for testing. 
     */
    fun getUserByName(ctx: Context) {
        val result = service.getUserByName(ctx.pathParamAsClass<kotlin.String>("username").get(), ctx)
        ctx.status(200).json(result)
    }

    /**
     * Logs user into the system
     * 
     * @param username The user name for login 
     * @param password The password for login in clear text 
     */
    fun loginUser(ctx: Context) {
        val result = service.loginUser(ctx.queryParamAsClass<kotlin.String>("username").get(), ctx.queryParamAsClass<kotlin.String>("password").get(), ctx)
        ctx.status(200).json(result)
    }

    /**
     * Logs out current logged in user session
     * 
     */
    fun logoutUser(ctx: Context) {
        val result = service.logoutUser(ctx)
        ctx.status(200).json(result)
    }

    /**
     * Updated user
     * This can only be done by the logged in user.
     * @param username name that need to be deleted 
     * @param user Updated user object 
     */
    fun updateUser(ctx: Context) {
        val result = service.updateUser(ctx.pathParamAsClass<kotlin.String>("username").get(), ctx.bodyAsClass<User>(), ctx)
        ctx.status(400).json(result)
    }

}
