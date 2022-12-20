//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_collection/built_collection.dart';
import 'package:openapi/src/model/category.dart';
import 'package:openapi/src/model/tag.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'pet.g.dart';

/// Pet
///
/// Properties:
/// * [id] 
/// * [category] 
/// * [name] 
/// * [photoUrls] 
/// * [tags] 
/// * [status] - pet status in the store
@BuiltValue()
abstract class Pet implements Built<Pet, PetBuilder> {
  @BuiltValueField(wireName: r'id')
  int? get id;

  @BuiltValueField(wireName: r'category')
  Category? get category;

  @BuiltValueField(wireName: r'name')
  String get name;

  @BuiltValueField(wireName: r'photoUrls')
  BuiltSet<String> get photoUrls;

  @BuiltValueField(wireName: r'tags')
  BuiltList<Tag>? get tags;

  /// pet status in the store
  @BuiltValueField(wireName: r'status')
  PetStatusEnum? get status;
  // enum statusEnum {  available,  pending,  sold,  };

  Pet._();

  factory Pet([void updates(PetBuilder b)]) = _$Pet;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(PetBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<Pet> get serializer => _$PetSerializer();
}

class _$PetSerializer implements PrimitiveSerializer<Pet> {
  @override
  final Iterable<Type> types = const [Pet, _$Pet];

  @override
  final String wireName = r'Pet';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    Pet object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.id != null) {
      yield r'id';
      yield serializers.serialize(
        object.id,
        specifiedType: const FullType(int),
      );
    }
    if (object.category != null) {
      yield r'category';
      yield serializers.serialize(
        object.category,
        specifiedType: const FullType(Category),
      );
    }
    yield r'name';
    yield serializers.serialize(
      object.name,
      specifiedType: const FullType(String),
    );
    yield r'photoUrls';
    yield serializers.serialize(
      object.photoUrls,
      specifiedType: const FullType(BuiltSet, [FullType(String)]),
    );
    if (object.tags != null) {
      yield r'tags';
      yield serializers.serialize(
        object.tags,
        specifiedType: const FullType(BuiltList, [FullType(Tag)]),
      );
    }
    if (object.status != null) {
      yield r'status';
      yield serializers.serialize(
        object.status,
        specifiedType: const FullType(PetStatusEnum),
      );
    }
  }

  @override
  Object serialize(
    Serializers serializers,
    Pet object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required PetBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'id':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(int),
          ) as int;
          result.id = valueDes;
          break;
        case r'category':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(Category),
          ) as Category;
          result.category.replace(valueDes);
          break;
        case r'name':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.name = valueDes;
          break;
        case r'photoUrls':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(BuiltSet, [FullType(String)]),
          ) as BuiltSet<String>;
          result.photoUrls.replace(valueDes);
          break;
        case r'tags':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(BuiltList, [FullType(Tag)]),
          ) as BuiltList<Tag>;
          result.tags.replace(valueDes);
          break;
        case r'status':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(PetStatusEnum),
          ) as PetStatusEnum;
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
  Pet deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = PetBuilder();
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

class PetStatusEnum extends EnumClass {

  /// pet status in the store
  @BuiltValueEnumConst(wireName: r'available')
  static const PetStatusEnum available = _$petStatusEnum_available;
  /// pet status in the store
  @BuiltValueEnumConst(wireName: r'pending')
  static const PetStatusEnum pending = _$petStatusEnum_pending;
  /// pet status in the store
  @BuiltValueEnumConst(wireName: r'sold')
  static const PetStatusEnum sold = _$petStatusEnum_sold;
  /// pet status in the store
  @BuiltValueEnumConst(wireName: r'unknown_default_open_api', fallback: true)
  static const PetStatusEnum unknownDefaultOpenApi = _$petStatusEnum_unknownDefaultOpenApi;

  static Serializer<PetStatusEnum> get serializer => _$petStatusEnumSerializer;

  const PetStatusEnum._(String name): super(name);

  static BuiltSet<PetStatusEnum> get values => _$petStatusEnumValues;
  static PetStatusEnum valueOf(String name) => _$petStatusEnumValueOf(name);
}

