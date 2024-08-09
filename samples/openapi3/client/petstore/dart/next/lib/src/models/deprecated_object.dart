// Model def

import 'package:petstore_api/_internal.dart';


part 'deprecated_object.reflection.dart';
part 'deprecated_object.serialization.dart';


/// DeprecatedObjectMixin
///
/// Properties:
/// * [name] 
@Deprecated('DeprecatedObjectMixin has been deprecated')
mixin DeprecatedObjectMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            String

> get name;
  
}

/// DeprecatedObject
///
/// Properties:
/// * [name] 
@Deprecated('DeprecatedObjectMixin has been deprecated')
class DeprecatedObject with
$OpenApiObjectMixin,

DeprecatedObjectMixin {
  @override
  UndefinedWrapper<
            String

> name;

  AdditionalProperties<Object

?> additionalProperties;

  

  DeprecatedObject.$all({
        required this.name,
    required this.additionalProperties,
    
  });

  DeprecatedObject({
      this.name = const UndefinedWrapper
        .undefined()
,
    this.additionalProperties = const AdditionalProperties(),
    
  });

  static const $reflection = DeprecatedObjectReflection.instance;
  DeprecatedObjectReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  Map<String, dynamic> toMap() {
    return _$DeprecatedObjectToMap(this);
  }
  factory DeprecatedObject.fromMap(Map<String, dynamic> src) {
    return _$DeprecatedObjectFromMap(src);
  }
  static DeprecatedObject? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return DeprecatedObject.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$DeprecatedObjectCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory DeprecatedObject.deserialize(Object? src) {
    return _$DeprecatedObjectDeserialize(src);
  }
  static DeprecatedObject? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return DeprecatedObject.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$DeprecatedObjectCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Map<String,dynamic> serialize() {
    return _$DeprecatedObjectSerialize(this);
  }
}




