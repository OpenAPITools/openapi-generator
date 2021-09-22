package org.openapitools.generator.gradle.plugin.service

import org.gradle.api.provider.Property

class PropertyHandler {
    companion object {
        fun <T : Any?> Property<T>.ifNotEmpty(block: Property<T>.(T) -> Unit) {
            if (isPresent) {
                val item: T? = get()
                if (item != null) {
                    when (get()) {
                        is String -> if ((get() as String).isNotEmpty()) {
                            block(get())
                        }
                        is String? -> if (true == (get() as String?)?.isNotEmpty()) {
                            block(get())
                        }
                        else -> block(get())
                    }
                }
            }
        }
    }
}
