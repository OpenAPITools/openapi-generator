package org.openapitools.api

import org.openapitools.model.User
import jakarta.validation.Valid
import jakarta.validation.constraints.DecimalMax
import jakarta.validation.constraints.DecimalMin
import jakarta.validation.constraints.Email
import jakarta.validation.constraints.Max
import jakarta.validation.constraints.Min
import jakarta.validation.constraints.NotNull
import jakarta.validation.constraints.Pattern
import jakarta.validation.constraints.Size
import org.springframework.stereotype.Service
@Service
class UserApiServiceImpl : UserApiService {

    override fun createUser(user: User): Unit {
        TODO("Implement me")
    }

    override fun createUsersWithArrayInput(user: kotlin.collections.List<@Valid User>): Unit {
        TODO("Implement me")
    }

    override fun createUsersWithListInput(user: kotlin.collections.List<@Valid User>): Unit {
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

    override fun updateUser(username: kotlin.String, user: User): Unit {
        TODO("Implement me")
    }
}
