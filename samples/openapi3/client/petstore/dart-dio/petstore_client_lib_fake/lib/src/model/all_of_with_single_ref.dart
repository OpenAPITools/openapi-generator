//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/single_ref_type.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'all_of_with_single_ref.g.dart';

/// AllOfWithSingleRef
///
/// Properties:
/// * [username] 
/// * [singleRefType] 
@BuiltValue()
abstract class AllOfWithSingleRef implements Built<AllOfWithSingleRef, AllOfWithSingleRefBuilder> {
  @BuiltValueField(wireName: r'username')
  String? get username;

  @BuiltValueField(wireName: r'SingleRefType')
  SingleRefType? get singleRefType;
  // enum singleRefTypeEnum {  admin,  user,  };

  AllOfWithSingleRef._();

  factory AllOfWithSingleRef([void updates(AllOfWithSingleRefBuilder b)]) = _$AllOfWithSingleRef;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(AllOfWithSingleRefBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<AllOfWithSingleRef> get serializer => _$AllOfWithSingleRefSerializer();
}

class _$AllOfWithSingleRefSerializer implements PrimitiveSerializer<AllOfWithSingleRef> {
  @override
  final Iterable<Type> types = const [AllOfWithSingleRef, _$AllOfWithSingleRef];

  @override
  final String wireName = r'AllOfWithSingleRef';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    AllOfWithSingleRef object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.username != null) {
      yield r'username';
      yield serializers.serialize(
        object.username,
        specifiedType: const FullType(String),
      );
    }
    if (object.singleRefType != null) {
      yield r'SingleRefType';
      yield serializers.serialize(
        object.singleRefType,
        specifiedType: const FullType(SingleRefType),
      );
    }
  }

  @override
  Object serialize(
    Serializers serializers,
    AllOfWithSingleRef object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required AllOfWithSingleRefBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'username':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.username = valueDes;
          break;
        case r'SingleRefType':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(SingleRefType),
          ) as SingleRefType;
          result.singleRefType = valueDes;
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  AllOfWithSingleRef deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = AllOfWithSingleRefBuilder();
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

