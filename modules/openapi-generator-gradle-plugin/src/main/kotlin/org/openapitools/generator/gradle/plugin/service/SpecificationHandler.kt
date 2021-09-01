package org.openapitools.generator.gradle.plugin.service

import org.gradle.api.GradleException
import org.gradle.api.provider.Property
import org.openapitools.generator.gradle.plugin.service.PropertyHandler.Companion.ifNotEmpty
import java.net.URI
import java.net.URL

class SpecificationHandler {
    companion object {
        fun getInputSpec(inputSpec : Property<String>,
                         remoteSpec : Property<String?>): String {
            val specificationParameters = SpecificationParameters()

            inputSpec.ifNotEmpty { value ->
                specificationParameters.localSpec = value
            }

            remoteSpec.ifNotEmpty { value ->
                specificationParameters.remoteSpec = checkForValidUrlAndReturn(value)
            }

            return specificationParameters.getInputSpec() ?: throw GradleException("Specification path is undefined")
        }

        private fun checkForValidUrlAndReturn(value: String?): String? {
            return if (value == null) {
                null
            } else {
                toURL(value).toString()
            }
        }

        private fun toURL(value: String): URL? {
            try {
                return URI(value).toURL()
            } catch (ex: Exception) {
                throw GradleException("Remote spec has invalid url format: $value", ex)
            }
        }
    }

    private class SpecificationParameters {
        var localSpec: String? = null
        var remoteSpec: String? = null

        fun getInputSpec(): String? {
            return localSpec ?: remoteSpec;
        }
    }
}
