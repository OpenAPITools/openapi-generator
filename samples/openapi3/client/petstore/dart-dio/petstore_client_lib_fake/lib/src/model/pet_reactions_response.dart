//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_collection/built_collection.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'pet_reactions_response.g.dart';

/// Reproduces a bug where nested `Map<String, Map<String, T>>` properties were decoded with a single-level `mapCastOfType<String, dynamic>` cast that can't be assigned to the declared field type. 
///
/// Properties:
/// * [myReacts] 
/// * [reactionCounts] 
@BuiltValue()
abstract class PetReactionsResponse implements Built<PetReactionsResponse, PetReactionsResponseBuilder> {
  @BuiltValueField(wireName: r'myReacts')
  BuiltMap<String, BuiltMap<String, bool>>? get myReacts;

  @BuiltValueField(wireName: r'reactionCounts')
  BuiltMap<String, BuiltMap<String, int>>? get reactionCounts;

  PetReactionsResponse._();

  factory PetReactionsResponse([void updates(PetReactionsResponseBuilder b)]) = _$PetReactionsResponse;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(PetReactionsResponseBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<PetReactionsResponse> get serializer => _$PetReactionsResponseSerializer();
}

class _$PetReactionsResponseSerializer implements PrimitiveSerializer<PetReactionsResponse> {
  @override
  final Iterable<Type> types = const [PetReactionsResponse, _$PetReactionsResponse];

  @override
  final String wireName = r'PetReactionsResponse';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    PetReactionsResponse object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.myReacts != null) {
      yield r'myReacts';
      yield serializers.serialize(
        object.myReacts,
        specifiedType: const FullType(BuiltMap, [FullType(String), FullType(BuiltMap, [FullType(String), FullType(bool)])]),
      );
    }
    if (object.reactionCounts != null) {
      yield r'reactionCounts';
      yield serializers.serialize(
        object.reactionCounts,
        specifiedType: const FullType(BuiltMap, [FullType(String), FullType(BuiltMap, [FullType(String), FullType(int)])]),
      );
    }
  }

  @override
  Object serialize(
    Serializers serializers,
    PetReactionsResponse object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required PetReactionsResponseBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'myReacts':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(BuiltMap, [FullType(String), FullType(BuiltMap, [FullType(String), FullType(bool)])]),
          ) as BuiltMap<String, BuiltMap<String, bool>>;
          result.myReacts.replace(valueDes);
          break;
        case r'reactionCounts':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(BuiltMap, [FullType(String), FullType(BuiltMap, [FullType(String), FullType(int)])]),
          ) as BuiltMap<String, BuiltMap<String, int>>;
          result.reactionCounts.replace(valueDes);
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  PetReactionsResponse deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = PetReactionsResponseBuilder();
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

