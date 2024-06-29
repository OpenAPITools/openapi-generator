// Model def

import 'package:openapi/_internal.dart';


part 'array_of_inline_all_of.reflection.dart';
part 'array_of_inline_all_of.serialization.dart';


/// ArrayOfInlineAllOfMixin
///
/// Properties:
/// * [id] 
/// * [name] 
/// * [arrayAllofDogProperty] 
mixin ArrayOfInlineAllOfMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            int
> get id;

            String
 get name;
UndefinedWrapper<
    List<
        
            ArrayOfInlineAllOfArrayAllofDogPropertyInner
>
> get arrayAllofDogProperty;
  
}

/// ArrayOfInlineAllOf
///
/// Properties:
/// * [id] 
/// * [name] 
/// * [arrayAllofDogProperty] 
class ArrayOfInlineAllOf with
$OpenApiObjectMixin,


ArrayOfInlineAllOfMixin {
  @override
  UndefinedWrapper<
            int
> id;
  @override
  
            String
 name;
  @override
  UndefinedWrapper<
    List<
        
            ArrayOfInlineAllOfArrayAllofDogPropertyInner
>
> arrayAllofDogProperty;

  AdditionalProperties<Object
?> additionalProperties;

  

  ArrayOfInlineAllOf.$all({
        required this.id,
    required this.name,
    required this.arrayAllofDogProperty,
    required this.additionalProperties,
    
  });

  ArrayOfInlineAllOf({
      this.id = const UndefinedWrapper
        .undefined()
,
required  this.name     ,
  this.arrayAllofDogProperty = const UndefinedWrapper
        .undefined()
,
    this.additionalProperties = const AdditionalProperties(),
    
  });

  static const $reflection = ArrayOfInlineAllOfReflection.instance;

  @override
  bool validate() {
    return super.validate();
  }


  Map<String, dynamic> toMap() {
    return _$ArrayOfInlineAllOfToMap(this);
  }
  factory ArrayOfInlineAllOf.fromMap(Map<String, dynamic> src) {
    return _$ArrayOfInlineAllOfFromMap(src);
  }
  static ArrayOfInlineAllOf? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return ArrayOfInlineAllOf.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$ArrayOfInlineAllOfCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory ArrayOfInlineAllOf.deserialize(Object? src) {
    return _$ArrayOfInlineAllOfDeserialize(src);
  }
  static ArrayOfInlineAllOf? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return ArrayOfInlineAllOf.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$ArrayOfInlineAllOfCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Map<String,dynamic> serialize() {
    return _$ArrayOfInlineAllOfSerialize(this);
  }
}




