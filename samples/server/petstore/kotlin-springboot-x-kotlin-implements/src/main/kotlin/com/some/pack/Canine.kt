package com.some.pack

import org.openapitools.model.Dog

interface Canine {

    val breed: Dog.Breed
    val bark: kotlin.Boolean

}