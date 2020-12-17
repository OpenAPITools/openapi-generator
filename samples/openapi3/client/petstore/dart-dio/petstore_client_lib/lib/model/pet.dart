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

    @nullable
    @BuiltValueField(wireName: r'name')
    String get name;

    @nullable
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

    // Boilerplate code needed to wire-up generated code
    Pet._();

    factory Pet([updates(PetBuilder b)]) = _$Pet;
    static Serializer<Pet> get serializer => _$petSerializer;
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

