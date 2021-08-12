package org.openapitools.model

import com.fasterxml.jackson.annotation.JsonSubTypes
import com.fasterxml.jackson.annotation.JsonTypeInfo
import io.swagger.annotations.ApiModel
import io.swagger.annotations.ApiModelProperty
import io.micronaut.test.extensions.spock.annotation.MicronautTest
import spock.lang.Specification
import jakarta.inject.Inject

/**
 * Model tests for Animal
 */
@MicronautTest
public class AnimalSpec extends Specification {
    private final Animal model = new Animal()

    /**
     * Model tests for Animal
     */
    void "Animal test"() {
        // TODO: test Animal
    }

    /**
     * Test the property 'className'
     */
    void "Animal property className test"() {
        // TODO: test className
    }

    /**
     * Test the property 'color'
     */
    void "Animal property color test"() {
        // TODO: test color
    }

}
