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
    /* pet status in the store */
        @nullable
    @BuiltValueField(wireName: r'status')
    PetStatus get status;
        //enum statusEnum {  available,  pending,  sold,  };

    // Boilerplate code needed to wire-up generated code
    Pet._();

    factory Pet([updates(PetBuilder b)]) = _$Pet;
    static Serializer<Pet> get serializer => _$petSerializer;
}

class PetStatus extends EnumClass {

  /// pet status in the store
  @BuiltValueEnumConst(wireName: "available")
  static const PetStatus available = _$available;
  /// pet status in the store
  @BuiltValueEnumConst(wireName: "pending")
  static const PetStatus pending = _$pending;
  /// pet status in the store
  @BuiltValueEnumConst(wireName: "sold")
  static const PetStatus sold = _$sold;

  static Serializer<PetStatus> get serializer => _$petStatusSerializer;

  const PetStatus._(String name): super(name);

  static BuiltSet<PetStatus> get values => _$petStatusValues;
  static PetStatus valueOf(String name) => _$petStatusValueOf(name);
}


