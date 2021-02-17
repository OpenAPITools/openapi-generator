//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.7

// ignore_for_file: unused_import

import 'package:openapi/model/tag.dart';
import 'package:built_collection/built_collection.dart';
import 'package:openapi/model/category.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'pet.g.dart';

abstract class Pet implements Built<Pet, PetBuilder> {

    @nullable
    @BuiltValueField(wireName: r'id')
    int get id;

    @nullable
    @BuiltValueField(wireName: r'category')
    Category get category;

    @BuiltValueField(wireName: r'name')
    String get name;

    @BuiltValueField(wireName: r'photoUrls')
    BuiltList<String> get photoUrls;

    @nullable
    @BuiltValueField(wireName: r'tags')
    BuiltList<Tag> get tags;

    /// pet status in the store
    @nullable
    @BuiltValueField(wireName: r'status')
    PetStatusEnum get status;
    // enum statusEnum {  available,  pending,  sold,  };

    Pet._();

    static void _initializeBuilder(PetBuilder b) => b;

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
    Iterable<Object> serialize(Serializers serializers, Pet object,
        {FullType specifiedType = FullType.unspecified}) {
        final result = <Object>[];
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
                specifiedType: const FullType(BuiltList, [FullType(String)])));
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
    Pet deserialize(Serializers serializers, Iterable<Object> serialized,
        {FullType specifiedType = FullType.unspecified}) {
        final result = PetBuilder();

        final iterator = serialized.iterator;
        while (iterator.moveNext()) {
            final key = iterator.current as String;
            iterator.moveNext();
            final dynamic value = iterator.current;
            switch (key) {
                case r'id':
                    result.id = serializers.deserialize(value,
                        specifiedType: const FullType(int)) as int;
                    break;
                case r'category':
                    result.category.replace(serializers.deserialize(value,
                        specifiedType: const FullType(Category)) as Category);
                    break;
                case r'name':
                    result.name = serializers.deserialize(value,
                        specifiedType: const FullType(String)) as String;
                    break;
                case r'photoUrls':
                    result.photoUrls.replace(serializers.deserialize(value,
                        specifiedType: const FullType(BuiltList, [FullType(String)])) as BuiltList<String>);
                    break;
                case r'tags':
                    result.tags.replace(serializers.deserialize(value,
                        specifiedType: const FullType(BuiltList, [FullType(Tag)])) as BuiltList<Tag>);
                    break;
                case r'status':
                    result.status = serializers.deserialize(value,
                        specifiedType: const FullType(PetStatusEnum)) as PetStatusEnum;
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

