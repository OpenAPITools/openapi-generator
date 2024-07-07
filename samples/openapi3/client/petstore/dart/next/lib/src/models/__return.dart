// Model def

import 'package:petstore_api/_internal.dart';


part '__return.reflection.dart';
part '__return.serialization.dart';


/// Model for testing reserved words
///
/// Properties:
/// * [$return] 
mixin $ReturnMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            int
> get $return;
  
}

/// Model for testing reserved words
///
/// Properties:
/// * [$return] 
class $Return with
$OpenApiObjectMixin,

$ReturnMixin {
  @override
  UndefinedWrapper<
            int
> $return;

  AdditionalProperties<Object
?> additionalProperties;

  

  $Return.$all({
        required this.$return,
    required this.additionalProperties,
    
  });

  $Return({
      this.$return = const UndefinedWrapper
        .undefined()
,
    this.additionalProperties = const AdditionalProperties(),
    
  });

  static const $reflection = $ReturnReflection.instance;
  $ReturnReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  Map<String, dynamic> toMap() {
    return _$$ReturnToMap(this);
  }
  factory $Return.fromMap(Map<String, dynamic> src) {
    return _$$ReturnFromMap(src);
  }
  static $Return? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return $Return.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$$ReturnCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory $Return.deserialize(Object? src) {
    return _$$ReturnDeserialize(src);
  }
  static $Return? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return $Return.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$$ReturnCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Map<String,dynamic> serialize() {
    return _$$ReturnSerialize(this);
  }
}




