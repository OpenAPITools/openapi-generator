//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'pet_reaction_response.g.dart';

/// Reproduces a bug where anyOf between a named-enum $ref and an inline literal-enum produced a property typed as an undeclared enum class. Generator must either inline a proper enum or fall back to String. 
///
/// Properties:
/// * [petId] 
/// * [status] 
@BuiltValue()
abstract class PetReactionResponse implements Built<PetReactionResponse, PetReactionResponseBuilder> {
  @BuiltValueField(wireName: r'petId')
  int? get petId;

  @BuiltValueField(wireName: r'status')
  String? get status;

  PetReactionResponse._();

  factory PetReactionResponse([void updates(PetReactionResponseBuilder b)]) = _$PetReactionResponse;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(PetReactionResponseBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<PetReactionResponse> get serializer => _$PetReactionResponseSerializer();
}

class _$PetReactionResponseSerializer implements PrimitiveSerializer<PetReactionResponse> {
  @override
  final Iterable<Type> types = const [PetReactionResponse, _$PetReactionResponse];

  @override
  final String wireName = r'PetReactionResponse';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    PetReactionResponse object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.petId != null) {
      yield r'petId';
      yield serializers.serialize(
        object.petId,
        specifiedType: const FullType(int),
      );
    }
    if (object.status != null) {
      yield r'status';
      yield serializers.serialize(
        object.status,
        specifiedType: const FullType(String),
      );
    }
  }

  @override
  Object serialize(
    Serializers serializers,
    PetReactionResponse object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required PetReactionResponseBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'petId':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(int),
          ) as int;
          result.petId = valueDes;
          break;
        case r'status':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.status = valueDes;
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  PetReactionResponse deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = PetReactionResponseBuilder();
    final serializedList = (serialized as Iterable<Object?>).toList();
    final unhandled = <Object?>[];
    _deserializeProperties(
      serializers,
      serialized,
      specifiedType: specifiedType,
      serializedList: serializedList,
      unhandled: unhandled,
      result: result,
    );
    return result.build();
  }
}

