// Model def

import 'package:openapi/_internal.dart';


part 'array_of_array_of_number_only.reflection.dart';
part 'array_of_array_of_number_only.serialization.dart';


/// ArrayOfArrayOfNumberOnlyMixin
///
/// Properties:
/// * [arrayArrayNumber] 
mixin ArrayOfArrayOfNumberOnlyMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
    List<
        
    List<
        
            num
>
>
> get arrayArrayNumber;
  
}

/// ArrayOfArrayOfNumberOnly
///
/// Properties:
/// * [arrayArrayNumber] 
class ArrayOfArrayOfNumberOnly with
$OpenApiObjectMixin,


ArrayOfArrayOfNumberOnlyMixin {
  @override
  UndefinedWrapper<
    List<
        
    List<
        
            num
>
>
> arrayArrayNumber;

  AdditionalProperties<Object
?> additionalProperties;

  

  ArrayOfArrayOfNumberOnly.$all({
        required this.arrayArrayNumber,
    required this.additionalProperties,
    
  });

  ArrayOfArrayOfNumberOnly({
      this.arrayArrayNumber = const UndefinedWrapper
        .undefined()
,
    this.additionalProperties = const AdditionalProperties(),
    
  });

  static const $reflection = ArrayOfArrayOfNumberOnlyReflection.instance;

  @override
  bool validate() {
    return super.validate();
  }


  Map<String, dynamic> toMap() {
    return _$ArrayOfArrayOfNumberOnlyToMap(this);
  }
  factory ArrayOfArrayOfNumberOnly.fromMap(Map<String, dynamic> src) {
    return _$ArrayOfArrayOfNumberOnlyFromMap(src);
  }
  static ArrayOfArrayOfNumberOnly? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return ArrayOfArrayOfNumberOnly.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$ArrayOfArrayOfNumberOnlyCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory ArrayOfArrayOfNumberOnly.deserialize(Object? src) {
    return _$ArrayOfArrayOfNumberOnlyDeserialize(src);
  }
  static ArrayOfArrayOfNumberOnly? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return ArrayOfArrayOfNumberOnly.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$ArrayOfArrayOfNumberOnlyCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Map<String,dynamic> serialize() {
    return _$ArrayOfArrayOfNumberOnlySerialize(this);
  }
}




