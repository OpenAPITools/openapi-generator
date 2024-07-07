// Model def

import 'package:petstore_api/_internal.dart';


part 'array_of_number_only.reflection.dart';
part 'array_of_number_only.serialization.dart';


/// ArrayOfNumberOnlyMixin
///
/// Properties:
/// * [arrayNumber] 
mixin ArrayOfNumberOnlyMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
    List<
        
            num
>
> get arrayNumber;
  
}

/// ArrayOfNumberOnly
///
/// Properties:
/// * [arrayNumber] 
class ArrayOfNumberOnly with
$OpenApiObjectMixin,

ArrayOfNumberOnlyMixin {
  @override
  UndefinedWrapper<
    List<
        
            num
>
> arrayNumber;

  AdditionalProperties<Object
?> additionalProperties;

  

  ArrayOfNumberOnly.$all({
        required this.arrayNumber,
    required this.additionalProperties,
    
  });

  ArrayOfNumberOnly({
      this.arrayNumber = const UndefinedWrapper
        .undefined()
,
    this.additionalProperties = const AdditionalProperties(),
    
  });

  static const $reflection = ArrayOfNumberOnlyReflection.instance;
  ArrayOfNumberOnlyReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  Map<String, dynamic> toMap() {
    return _$ArrayOfNumberOnlyToMap(this);
  }
  factory ArrayOfNumberOnly.fromMap(Map<String, dynamic> src) {
    return _$ArrayOfNumberOnlyFromMap(src);
  }
  static ArrayOfNumberOnly? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return ArrayOfNumberOnly.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$ArrayOfNumberOnlyCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory ArrayOfNumberOnly.deserialize(Object? src) {
    return _$ArrayOfNumberOnlyDeserialize(src);
  }
  static ArrayOfNumberOnly? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return ArrayOfNumberOnly.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$ArrayOfNumberOnlyCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Map<String,dynamic> serialize() {
    return _$ArrayOfNumberOnlySerialize(this);
  }
}




