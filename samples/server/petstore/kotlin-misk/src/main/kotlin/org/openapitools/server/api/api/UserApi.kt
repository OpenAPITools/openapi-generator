package org.openapitools.server.api.api

import org.openapitools.server.api.model.User
import okhttp3.Headers

interface UserApi {

    fun createUser(@Valid @RequestBody user: User) {

    fun createUsersWithArrayInput(@Valid @RequestBody user: kotlin.Array<User>) {

    fun createUsersWithListInput(@Valid @RequestBody user: kotlin.Array<User>) {

    fun deleteUser(@PathParam("username") username: kotlin.String) {

    fun getUserByName(@PathParam("username") username: kotlin.String): User {

    fun loginUser( @QueryParam(value = "username") username: kotlin.String,  @QueryParam(value = "password") password: kotlin.String): kotlin.String {

    fun logoutUser() {

    fun updateUser(@PathParam("username") username: kotlin.String, @Valid @RequestBody user: User) {
}
