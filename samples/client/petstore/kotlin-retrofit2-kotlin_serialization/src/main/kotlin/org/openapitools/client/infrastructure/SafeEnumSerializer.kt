package org.openapitools.client.infrastructure

import kotlinx.serialization.Decoder
import kotlinx.serialization.Encoder
import kotlinx.serialization.KSerializer
import kotlinx.serialization.Serializer
import kotlinx.serialization.SerialDescriptor
import kotlinx.serialization.PrimitiveDescriptor
import kotlinx.serialization.PrimitiveKind
import kotlinx.serialization.enumMembers
import org.openapitools.client.infrastructure.SafeEnum.Companion.UNKNOWN_VALUE
import kotlin.reflect.KClass

interface SafeEnum {
    val serialName: String

    companion object {
        const val UNKNOWN_VALUE = "unknown_value"
    }
}

@Serializer(forClass = SafeEnum::class)
open class SafeEnumAdapter<E>(
    private val enumKClass: KClass<E>
) : KSerializer<E> where  E : Enum<E>, E : SafeEnum {
    private val declaredValues = enumKClass.enumMembers().associateBy { it.serialName }

    override val descriptor: SerialDescriptor = PrimitiveDescriptor("SafeEnum", PrimitiveKind.STRING)

    override fun serialize(encoder: Encoder, value: E) {
        encoder.encodeString(value.serialName)
    }

    override fun deserialize(decoder: Decoder): E = decoder.decodeString().let { value ->
        declaredValues[value]
            ?: declaredValues[UNKNOWN_VALUE]
            ?: throw IllegalStateException(
                """Cannot find enum with serialName='$value', add following default 'UNKNOWN_VALUE' enum to have backward compatibility:
                            |
                            |@Serializable(with = ${enumKClass.simpleName}Adapter::class)
                            |enum class ${enumKClass.simpleName}(override val value: String) : SerializableEnum {
                            |   ${enumKClass.enumMembers().joinToString("\n   ") { "${it.name}(\"${it.serialName}\")," }}
                            |   UNKNOWN_VALUE(UNKNOWN_VALUE); <--- HERE
                            |}""".trimMargin()
            )
    }
}