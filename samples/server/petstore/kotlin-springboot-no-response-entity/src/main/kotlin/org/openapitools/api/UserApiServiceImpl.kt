package org.openapitools.api

import org.openapitools.model.User
import javax.validation.Valid
import javax.validation.constraints.DecimalMax
import javax.validation.constraints.DecimalMin
import javax.validation.constraints.Email
import javax.validation.constraints.Max
import javax.validation.constraints.Min
import javax.validation.constraints.NotNull
import javax.validation.constraints.Pattern
import javax.validation.constraints.Size
import org.springframework.stereotype.Service
@Service
class UserApiServiceImpl : UserApiService {

    override fun createUser(body: User): Unit {
        TODO("Implement me")
    }

    override fun createUsersWithArrayInput(body: kotlin.collections.List<@Valid User>): Unit {
        TODO("Implement me")
    }

    override fun createUsersWithListInput(body: kotlin.collections.List<@Valid User>): Unit {
        TODO("Implement me")
    }

    override fun deleteUser(username: kotlin.String): Unit {
        TODO("Implement me")
    }

    override fun getUserByName(username: kotlin.String): User {
        TODO("Implement me")
    }

    override fun loginUser(username: kotlin.String, password: kotlin.String): kotlin.String {
        TODO("Implement me")
    }

    override fun logoutUser(): Unit {
        TODO("Implement me")
    }

    override fun updateUser(username: kotlin.String, body: User): Unit {
        TODO("Implement me")
    }
}
