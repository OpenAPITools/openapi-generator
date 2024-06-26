// Model def

import 'package:openapi/_internal.dart';


part 'array_of_inline_all_of_array_allof_dog_property_inner.reflection.dart';
part 'array_of_inline_all_of_array_allof_dog_property_inner.serialization.dart';


/// ArrayOfInlineAllOfArrayAllofDogPropertyInnerMixin
///
/// Properties:
/// * [breed] 
/// * [color] 
mixin ArrayOfInlineAllOfArrayAllofDogPropertyInnerMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            String
> get breed;
UndefinedWrapper<
            String
> get color;
  
}

/// ArrayOfInlineAllOfArrayAllofDogPropertyInner
///
/// Properties:
/// * [breed] 
/// * [color] 
class ArrayOfInlineAllOfArrayAllofDogPropertyInner with
$OpenApiObjectMixin,


ArrayOfInlineAllOfArrayAllofDogPropertyInnerMixin {
  @override
  UndefinedWrapper<
            String
> breed;
  @override
  UndefinedWrapper<
            String
> color;

  AdditionalProperties<Object
?> additionalProperties;

  

  ArrayOfInlineAllOfArrayAllofDogPropertyInner.$all({
        required this.breed,
    required this.color,
    required this.additionalProperties,
    
  });

  ArrayOfInlineAllOfArrayAllofDogPropertyInner({
      this.breed = const UndefinedWrapper
        .undefined()
,
  this.color = const UndefinedWrapper
        .undefined()
,
    this.additionalProperties = const AdditionalProperties(),
    
  });

  static const $reflection = ArrayOfInlineAllOfArrayAllofDogPropertyInnerReflection.instance;

  @override
  bool validate() {
      
      
    return super.validate();
  }


  Map<String, dynamic> toMap() {
    return _$ArrayOfInlineAllOfArrayAllofDogPropertyInnerToMap(this);
  }
  factory ArrayOfInlineAllOfArrayAllofDogPropertyInner.fromMap(Map<String, dynamic> src) {
    return _$ArrayOfInlineAllOfArrayAllofDogPropertyInnerFromMap(src);
  }
  static ArrayOfInlineAllOfArrayAllofDogPropertyInner? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return ArrayOfInlineAllOfArrayAllofDogPropertyInner.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$ArrayOfInlineAllOfArrayAllofDogPropertyInnerCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory ArrayOfInlineAllOfArrayAllofDogPropertyInner.deserialize(Object? src) {
    return _$ArrayOfInlineAllOfArrayAllofDogPropertyInnerDeserialize(src);
  }
  static ArrayOfInlineAllOfArrayAllofDogPropertyInner? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return ArrayOfInlineAllOfArrayAllofDogPropertyInner.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$ArrayOfInlineAllOfArrayAllofDogPropertyInnerCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Object? serialize() {
    return _$ArrayOfInlineAllOfArrayAllofDogPropertyInnerSerialize(this);
  }
}




