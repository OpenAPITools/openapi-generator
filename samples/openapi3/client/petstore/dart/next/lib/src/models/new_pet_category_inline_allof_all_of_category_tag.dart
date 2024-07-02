// Model def

import 'package:openapi/_internal.dart';


part 'new_pet_category_inline_allof_all_of_category_tag.reflection.dart';
part 'new_pet_category_inline_allof_all_of_category_tag.serialization.dart';


/// NewPetCategoryInlineAllofAllOfCategoryTagMixin
///
/// Properties:
/// * [id] 
/// * [name] 
mixin NewPetCategoryInlineAllofAllOfCategoryTagMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            int
> get id;
UndefinedWrapper<
            String
> get name;
  
}

/// NewPetCategoryInlineAllofAllOfCategoryTag
///
/// Properties:
/// * [id] 
/// * [name] 
class NewPetCategoryInlineAllofAllOfCategoryTag with
$OpenApiObjectMixin,


NewPetCategoryInlineAllofAllOfCategoryTagMixin {
  @override
  UndefinedWrapper<
            int
> id;
  @override
  UndefinedWrapper<
            String
> name;

  AdditionalProperties<Object
?> additionalProperties;

  

  NewPetCategoryInlineAllofAllOfCategoryTag.$all({
        required this.id,
    required this.name,
    required this.additionalProperties,
    
  });

  NewPetCategoryInlineAllofAllOfCategoryTag({
      this.id = const UndefinedWrapper
        .undefined()
,
  this.name = const UndefinedWrapper
        .undefined()
,
    this.additionalProperties = const AdditionalProperties(),
    
  });

  static const $reflection = NewPetCategoryInlineAllofAllOfCategoryTagReflection.instance;

  @override
  bool validate() {
    return super.validate();
  }


  Map<String, dynamic> toMap() {
    return _$NewPetCategoryInlineAllofAllOfCategoryTagToMap(this);
  }
  factory NewPetCategoryInlineAllofAllOfCategoryTag.fromMap(Map<String, dynamic> src) {
    return _$NewPetCategoryInlineAllofAllOfCategoryTagFromMap(src);
  }
  static NewPetCategoryInlineAllofAllOfCategoryTag? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return NewPetCategoryInlineAllofAllOfCategoryTag.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$NewPetCategoryInlineAllofAllOfCategoryTagCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory NewPetCategoryInlineAllofAllOfCategoryTag.deserialize(Object? src) {
    return _$NewPetCategoryInlineAllofAllOfCategoryTagDeserialize(src);
  }
  static NewPetCategoryInlineAllofAllOfCategoryTag? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return NewPetCategoryInlineAllofAllOfCategoryTag.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$NewPetCategoryInlineAllofAllOfCategoryTagCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Map<String,dynamic> serialize() {
    return _$NewPetCategoryInlineAllofAllOfCategoryTagSerialize(this);
  }
}




