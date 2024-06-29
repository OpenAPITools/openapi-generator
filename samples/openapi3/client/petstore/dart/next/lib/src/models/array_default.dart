// Model def

import 'package:openapi/_internal.dart';


part 'array_default.reflection.dart';
part 'array_default.serialization.dart';


/// ArrayDefaultMixin
///
/// Properties:
/// * [withDefaultEmptyBracket] 
/// * [withoutDefault] 
mixin ArrayDefaultMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
    List<
        
            String
>
> get withDefaultEmptyBracket;
UndefinedWrapper<
    List<
        
            String
>
> get withoutDefault;
  
}

/// ArrayDefault
///
/// Properties:
/// * [withDefaultEmptyBracket] 
/// * [withoutDefault] 
class ArrayDefault with
$OpenApiObjectMixin,


ArrayDefaultMixin {
  @override
  UndefinedWrapper<
    List<
        
            String
>
> withDefaultEmptyBracket;
  @override
  UndefinedWrapper<
    List<
        
            String
>
> withoutDefault;

  AdditionalProperties<Object
?> additionalProperties;

  

  ArrayDefault.$all({
        required this.withDefaultEmptyBracket,
    required this.withoutDefault,
    required this.additionalProperties,
    
  });

  ArrayDefault({
      this.withDefaultEmptyBracket = const UndefinedWrapper
    (
        
        []
    )
    
,
  this.withoutDefault = const UndefinedWrapper
        .undefined()
,
    this.additionalProperties = const AdditionalProperties(),
    
  });

  static const $reflection = ArrayDefaultReflection.instance;

  @override
  bool validate() {
    return super.validate();
  }


  Map<String, dynamic> toMap() {
    return _$ArrayDefaultToMap(this);
  }
  factory ArrayDefault.fromMap(Map<String, dynamic> src) {
    return _$ArrayDefaultFromMap(src);
  }
  static ArrayDefault? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return ArrayDefault.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$ArrayDefaultCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory ArrayDefault.deserialize(Object? src) {
    return _$ArrayDefaultDeserialize(src);
  }
  static ArrayDefault? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return ArrayDefault.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$ArrayDefaultCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Map<String,dynamic> serialize() {
    return _$ArrayDefaultSerialize(this);
  }
}




