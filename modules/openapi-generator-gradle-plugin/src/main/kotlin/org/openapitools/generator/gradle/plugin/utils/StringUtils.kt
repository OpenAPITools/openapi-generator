package org.openapitools.generator.gradle.plugin.utils

private val remoteUriRegex = "^[a-zA-Z][a-zA-Z0-9+\\-.]+?:.*".toRegex()

/**
 * Determines if a string is a remote URI (e.g., http:, jar:, s3:)
 * while safely ignoring 1-letter Windows drives (e.g., C:\).
 */
internal fun String.isRemoteUri(): Boolean {
    return this.matches(remoteUriRegex)
}
