package org.openapitools.client.infrastructure

import io.ktor.utils.io.core.*
import kotlin.experimental.and

private val digits = "0123456789abcdef".toCharArray()
private const val BASE64_ALPHABET = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
private const val BASE64_MASK: Byte = 0x3f
private const val BASE64_PAD = '='
private val BASE64_INVERSE_ALPHABET = IntArray(256) { BASE64_ALPHABET.indexOf(it.toChar()) }

private fun String.toCharArray(): CharArray = CharArray(length) { get(it) }
private fun ByteArray.clearFrom(from: Int) = (from until size).forEach { this[it] = 0 }
private fun Int.toBase64(): Char = BASE64_ALPHABET[this]
private fun Byte.fromBase64(): Byte = BASE64_INVERSE_ALPHABET[toInt() and 0xff].toByte() and BASE64_MASK
internal fun ByteArray.encodeBase64(): String = buildPacket { writeFully(this@encodeBase64) }.encodeBase64()
internal fun String.decodeBase64Bytes(): ByteArray = buildPacket { writeStringUtf8(dropLastWhile { it == BASE64_PAD }) }.decodeBase64Bytes().readBytes()

/**
 * Encode [bytes] as a HEX string with no spaces, newlines and `0x` prefixes.
 *
 * Taken from https://github.com/ktorio/ktor/blob/master/ktor-utils/common/src/io/ktor/util/Crypto.kt
 */
internal fun hex(bytes: ByteArray): String {
    val result = CharArray(bytes.size * 2)
    var resultIndex = 0
    val digits = digits

    for (element in bytes) {
        val b = element.toInt() and 0xff
        result[resultIndex++] = digits[b shr 4]
        result[resultIndex++] = digits[b and 0x0f]
    }

    return result.concatToString()
}

/**
 * Decode bytes from HEX string. It should be no spaces and `0x` prefixes.
 *
 * Taken from https://github.com/ktorio/ktor/blob/master/ktor-utils/common/src/io/ktor/util/Crypto.kt
 */
internal fun hex(s: String): ByteArray {
    val result = ByteArray(s.length / 2)
    for (idx in result.indices) {
        val srcIdx = idx * 2
        val high = s[srcIdx].toString().toInt(16) shl 4
        val low = s[srcIdx + 1].toString().toInt(16)
        result[idx] = (high or low).toByte()
    }

    return result
}

/**
 * Encode [ByteReadPacket] in base64 format.
 *
 * Taken from https://github.com/ktorio/ktor/blob/424d1d2cfaa3281302c60af9500f738c8c2fc846/ktor-utils/common/src/io/ktor/util/Base64.kt
 */
private fun ByteReadPacket.encodeBase64(): String = buildString {
    val data = ByteArray(3)
    while (remaining > 0) {
        val read = readAvailable(data)
        data.clearFrom(read)

        val padSize = (data.size - read) * 8 / 6
        val chunk = ((data[0].toInt() and 0xFF) shl 16) or
                ((data[1].toInt() and 0xFF) shl 8) or
                (data[2].toInt() and 0xFF)

        for (index in data.size downTo padSize) {
            val char = (chunk shr (6 * index)) and BASE64_MASK.toInt()
            append(char.toBase64())
        }

        repeat(padSize) { append(BASE64_PAD) }
    }
}

/**
 * Decode [ByteReadPacket] from base64 format
 *
 * Taken from https://github.com/ktorio/ktor/blob/424d1d2cfaa3281302c60af9500f738c8c2fc846/ktor-utils/common/src/io/ktor/util/Base64.kt
 */
private fun ByteReadPacket.decodeBase64Bytes(): Input = buildPacket {
    val data = ByteArray(4)

    while (remaining > 0) {
        val read = readAvailable(data)

        val chunk = data.foldIndexed(0) { index, result, current ->
            result or (current.fromBase64().toInt() shl ((3 - index) * 6))
        }

        for (index in data.size - 2 downTo (data.size - read)) {
            val origin = (chunk shr (8 * index)) and 0xff
            writeByte(origin.toByte())
        }
    }
}


