//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_collection/built_collection.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'get_user_info_response.g.dart';

/// GetUserInfoResponse
///
/// Properties:
/// * [email] - Email address of user, if available
/// * [scopes] - Auth-Scopes of the user, if available
@BuiltValue()
abstract class GetUserInfoResponse implements Built<GetUserInfoResponse, GetUserInfoResponseBuilder> {
  /// Email address of user, if available
  @BuiltValueField(wireName: r'email')
  String? get email;

  /// Auth-Scopes of the user, if available
  @BuiltValueField(wireName: r'scopes')
  BuiltList<GetUserInfoResponseScopesEnum>? get scopes;
  // enum scopesEnum {  app,  cms,  };

  GetUserInfoResponse._();

  factory GetUserInfoResponse([void updates(GetUserInfoResponseBuilder b)]) = _$GetUserInfoResponse;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(GetUserInfoResponseBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<GetUserInfoResponse> get serializer => _$GetUserInfoResponseSerializer();
}

class _$GetUserInfoResponseSerializer implements PrimitiveSerializer<GetUserInfoResponse> {
  @override
  final Iterable<Type> types = const [GetUserInfoResponse, _$GetUserInfoResponse];

  @override
  final String wireName = r'GetUserInfoResponse';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    GetUserInfoResponse object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.email != null) {
      yield r'email';
      yield serializers.serialize(
        object.email,
        specifiedType: const FullType(String),
      );
    }
    if (object.scopes != null) {
      yield r'scopes';
      yield serializers.serialize(
        object.scopes,
        specifiedType: const FullType(BuiltList, [FullType(GetUserInfoResponseScopesEnum)]),
      );
    }
  }

  @override
  Object serialize(
    Serializers serializers,
    GetUserInfoResponse object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required GetUserInfoResponseBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'email':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType.nullable(String),
          ) as String?;
          if (valueDes == null) continue;
          result.email = valueDes;
          break;
        case r'scopes':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType.nullable(BuiltList, [FullType(GetUserInfoResponseScopesEnum)]),
          ) as BuiltList<GetUserInfoResponseScopesEnum>?;
          if (valueDes == null) continue;
          result.scopes.replace(valueDes);
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  GetUserInfoResponse deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = GetUserInfoResponseBuilder();
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


class GetUserInfoResponseScopesEnum extends EnumClass {

  @BuiltValueEnumConst(wireName: r'app')
  static const GetUserInfoResponseScopesEnum app = _$getUserInfoResponseScopesEnum_app;
  @BuiltValueEnumConst(wireName: r'cms')
  static const GetUserInfoResponseScopesEnum cms = _$getUserInfoResponseScopesEnum_cms;
  @BuiltValueEnumConst(wireName: r'unknown_default_open_api', fallback: true)
  static const GetUserInfoResponseScopesEnum unknownDefaultOpenApi = _$getUserInfoResponseScopesEnum_unknownDefaultOpenApi;

  static Serializer<GetUserInfoResponseScopesEnum> get serializer => _$getUserInfoResponseScopesEnumSerializer;

  const GetUserInfoResponseScopesEnum._(String name): super(name);

  static BuiltSet<GetUserInfoResponseScopesEnum> get values => _$getUserInfoResponseScopesEnumValues;
  static GetUserInfoResponseScopesEnum valueOf(String name) => _$getUserInfoResponseScopesEnumValueOf(name);
}

