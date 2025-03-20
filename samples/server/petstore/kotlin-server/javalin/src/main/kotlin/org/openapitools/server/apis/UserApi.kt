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
        ctx.status(200).json(service.createUser(ctx.bodyAsClass<User>()))
    }

    /**
     * Creates list of users with given input array
     * 
     * @param user List of user object 
     */
    fun createUsersWithArrayInput(ctx: Context) {
        ctx.status(200).json(service.createUsersWithArrayInput(ctx.bodyAsClass<List<User>>()))
    }

    /**
     * Creates list of users with given input array
     * 
     * @param user List of user object 
     */
    fun createUsersWithListInput(ctx: Context) {
        ctx.status(200).json(service.createUsersWithListInput(ctx.bodyAsClass<List<User>>()))
    }

    /**
     * Delete user
     * This can only be done by the logged in user.
     * @param username The name that needs to be deleted 
     */
    fun deleteUser(ctx: Context) {
        ctx.status(200).json(service.deleteUser(ctx.pathParamAsClass<kotlin.String>("username").get()))
    }

    /**
     * Get user by user name
     * 
     * @param username The name that needs to be fetched. Use user1 for testing. 
     */
    fun getUserByName(ctx: Context) {
        ctx.status(200).json(service.getUserByName(ctx.pathParamAsClass<kotlin.String>("username").get()))
    }

    /**
     * Logs user into the system
     * 
     * @param username The user name for login 
     * @param password The password for login in clear text 
     */
    fun loginUser(ctx: Context) {
        ctx.status(200).json(service.loginUser(ctx.queryParamAsClass<String>("username").get(), ctx.queryParamAsClass<String>("password").get()))
    }

    /**
     * Logs out current logged in user session
     * 
     */
    fun logoutUser(ctx: Context) {
        ctx.status(200).json(service.logoutUser())
    }

    /**
     * Updated user
     * This can only be done by the logged in user.
     * @param username name that need to be deleted 
     * @param user Updated user object 
     */
    fun updateUser(ctx: Context) {
        ctx.status(200).json(service.updateUser(ctx.pathParamAsClass<kotlin.String>("username").get(), ctx.bodyAsClass<User>()))
    }

}
