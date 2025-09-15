package org.openapitools.api

import org.openapitools.model.User

interface UserApiService {

    /**
     * POST /user : Create user
     *
     * @param user  (required)
     * @return successful operation (status code 200)
     * @see UserApi#createUser
     */
    fun createUser(user: User): Unit

    /**
     * POST /user/createWithArray : Creates list of users with given input array
     *
     * @param user  (required)
     * @return successful operation (status code 200)
     * @see UserApi#createUsersWithArrayInput
     */
    fun createUsersWithArrayInput(user: kotlin.collections.List<User>): Unit

    /**
     * POST /user/createWithList : Creates list of users with given input array
     *
     * @param user  (required)
     * @return successful operation (status code 200)
     * @see UserApi#createUsersWithListInput
     */
    fun createUsersWithListInput(user: kotlin.collections.List<User>): Unit

    /**
     * DELETE /user/{username} : Delete user
     *
     * @param username  (required)
     * @return Invalid username supplied (status code 400)
     *         or User not found (status code 404)
     * @see UserApi#deleteUser
     */
    fun deleteUser(username: kotlin.String): Unit

    /**
     * GET /user/{username} : Get user by user name
     *
     * @param username  (required)
     * @return successful operation (status code 200)
     *         or Invalid username supplied (status code 400)
     *         or User not found (status code 404)
     * @see UserApi#getUserByName
     */
    fun getUserByName(username: kotlin.String): User

    /**
     * GET /user/login : Logs user into the system
     *
     * @param username  (required)
     * @param password  (required)
     * @return successful operation (status code 200)
     *         or Invalid username/password supplied (status code 400)
     * @see UserApi#loginUser
     */
    fun loginUser(username: kotlin.String, password: kotlin.String): kotlin.String

    /**
     * GET /user/logout : Logs out current logged in user session
     *
     * @return successful operation (status code 200)
     * @see UserApi#logoutUser
     */
    fun logoutUser(): Unit

    /**
     * PUT /user/{username} : Updated user
     *
     * @param username  (required)
     * @param user  (required)
     * @return Invalid user supplied (status code 400)
     *         or User not found (status code 404)
     * @see UserApi#updateUser
     */
    fun updateUser(username: kotlin.String, user: User): Unit
}
