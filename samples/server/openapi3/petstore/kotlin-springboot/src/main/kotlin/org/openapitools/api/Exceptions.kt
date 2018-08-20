package org.openapitools.api

import org.springframework.http.HttpStatus
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation.ControllerAdvice
import org.springframework.web.bind.annotation.ExceptionHandler
import javax.servlet.http.HttpServletRequest

sealed class ApiException(msg: String, val code: Int) : Exception(msg)

class NotFoundException(msg: String, code: Int = HttpStatus.NOT_FOUND.value()) : ApiException(msg, code)


@ControllerAdvice
class DefaultExceptionHandler {

    @ExceptionHandler(value = [NotFoundException::class])
    fun onNotFound(ex: NotFoundException, request: HttpServletRequest): ResponseEntity<Unit> =
        ResponseEntity(HttpStatus.valueOf(ex.code))

    @ExceptionHandler(value = [NotImplementedError::class])
    fun onNotImplemented(ex: NotImplementedError, request: HttpServletRequest): ResponseEntity<Unit> =
        ResponseEntity(HttpStatus.NOT_IMPLEMENTED)
}
