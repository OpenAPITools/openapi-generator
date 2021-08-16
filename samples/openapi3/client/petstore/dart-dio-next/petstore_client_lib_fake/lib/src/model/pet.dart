//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

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

    @BuiltValueHook(initializeBuilder: true)
    static void _defaults(PetBuilder b) => b;

    factory Pet([void updates(PetBuilder b)]) = _$Pet;

    @BuiltValueSerializer(custom: true)
    static Serializer<Pet> get serializer => _$PetSerializer();
}

class _$PetSerializer implements StructuredSerializer<Pet> {
    @override
    final Iterable<Type> types = const [Pet, _$Pet];

    @override
    final String wireName = r'Pet';

    @override
    Iterable<Object?> serialize(Serializers serializers, Pet object,
        {FullType specifiedType = FullType.unspecified}) {
        final result = <Object?>[];
        if (object.id != null) {
            result
                ..add(r'id')
                ..add(serializers.serialize(object.id,
                    specifiedType: const FullType(int)));
        }
        if (object.category != null) {
            result
                ..add(r'category')
                ..add(serializers.serialize(object.category,
                    specifiedType: const FullType(Category)));
        }
        result
            ..add(r'name')
            ..add(serializers.serialize(object.name,
                specifiedType: const FullType(String)));
        result
            ..add(r'photoUrls')
            ..add(serializers.serialize(object.photoUrls,
                specifiedType: const FullType(BuiltSet, [FullType(String)])));
        if (object.tags != null) {
            result
                ..add(r'tags')
                ..add(serializers.serialize(object.tags,
                    specifiedType: const FullType(BuiltList, [FullType(Tag)])));
        }
        if (object.status != null) {
            result
                ..add(r'status')
                ..add(serializers.serialize(object.status,
                    specifiedType: const FullType(PetStatusEnum)));
        }
        return result;
    }

    @override
    Pet deserialize(Serializers serializers, Iterable<Object?> serialized,
        {FullType specifiedType = FullType.unspecified}) {
        final result = PetBuilder();

        final iterator = serialized.iterator;
        while (iterator.moveNext()) {
            final key = iterator.current as String;
            iterator.moveNext();
            final Object? value = iterator.current;
            
            switch (key) {
                case r'id':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType(int)) as int;
                    result.id = valueDes;
                    break;
                case r'category':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType(Category)) as Category;
                    result.category.replace(valueDes);
                    break;
                case r'name':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType(String)) as String;
                    result.name = valueDes;
                    break;
                case r'photoUrls':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType(BuiltSet, [FullType(String)])) as BuiltSet<String>;
                    result.photoUrls.replace(valueDes);
                    break;
                case r'tags':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType(BuiltList, [FullType(Tag)])) as BuiltList<Tag>;
                    result.tags.replace(valueDes);
                    break;
                case r'status':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType(PetStatusEnum)) as PetStatusEnum;
                    result.status = valueDes;
                    break;
            }
        }
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

  static Serializer<PetStatusEnum> get serializer => _$petStatusEnumSerializer;

  const PetStatusEnum._(String name): super(name);

  static BuiltSet<PetStatusEnum> get values => _$petStatusEnumValues;
  static PetStatusEnum valueOf(String name) => _$petStatusEnumValueOf(name);
}

