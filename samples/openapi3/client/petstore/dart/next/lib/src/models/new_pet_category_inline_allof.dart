// Model def

import 'package:petstore_api/_internal.dart';


part 'new_pet_category_inline_allof.reflection.dart';
part 'new_pet_category_inline_allof.serialization.dart';


/// NewPetCategoryInlineAllofMixin
///
/// Properties:
/// * [id] 
/// * [name] 
/// * [categoryTag] 
mixin NewPetCategoryInlineAllofMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            int

> get id;

            String

 get name;
UndefinedWrapper<
            NewPetCategoryInlineAllofAllOfCategoryTag

> get categoryTag;
  
}

/// NewPetCategoryInlineAllof
///
/// Properties:
/// * [id] 
/// * [name] 
/// * [categoryTag] 
class NewPetCategoryInlineAllof with
$OpenApiObjectMixin,

NewPetCategoryInlineAllofMixin {
  @override
  UndefinedWrapper<
            int

> id;
  @override
  
            String

 name;
  @override
  UndefinedWrapper<
            NewPetCategoryInlineAllofAllOfCategoryTag

> categoryTag;

  AdditionalProperties<Object

?> additionalProperties;

  

  NewPetCategoryInlineAllof.$all({
        required this.id,
    required this.name,
    required this.categoryTag,
    required this.additionalProperties,
    
  });

  NewPetCategoryInlineAllof({
      this.id = const UndefinedWrapper
        .undefined()
,
  this.name     =
        
        'default-name'
        
,
  this.categoryTag = const UndefinedWrapper
        .undefined()
,
    this.additionalProperties = const AdditionalProperties(),
    
  });

  static const $reflection = NewPetCategoryInlineAllofReflection.instance;
  NewPetCategoryInlineAllofReflection get $classReflection => $reflection;

  @override
  bool validate() {
      
    return super.validate();
  }

  Map<String, dynamic> toMap() {
    return _$NewPetCategoryInlineAllofToMap(this);
  }
  factory NewPetCategoryInlineAllof.fromMap(Map<String, dynamic> src) {
    return _$NewPetCategoryInlineAllofFromMap(src);
  }
  static NewPetCategoryInlineAllof? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return NewPetCategoryInlineAllof.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$NewPetCategoryInlineAllofCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory NewPetCategoryInlineAllof.deserialize(Object? src) {
    return _$NewPetCategoryInlineAllofDeserialize(src);
  }
  static NewPetCategoryInlineAllof? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return NewPetCategoryInlineAllof.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$NewPetCategoryInlineAllofCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Map<String,dynamic> serialize() {
    return _$NewPetCategoryInlineAllofSerialize(this);
  }
}




