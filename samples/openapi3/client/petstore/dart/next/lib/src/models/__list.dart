// Model def

import 'package:openapi/_internal.dart';


part '__list.reflection.dart';
part '__list.serialization.dart';


/// $ListMixin
///
/// Properties:
/// * [$123list] 
mixin $ListMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            String
> get $123list;
  
}

/// $List
///
/// Properties:
/// * [$123list] 
class $List with
$OpenApiObjectMixin,


$ListMixin {
  @override
  UndefinedWrapper<
            String
> $123list;

  AdditionalProperties<Object
?> additionalProperties;

  

  $List.$all({
        required this.$123list,
    required this.additionalProperties,
    
  });

  $List({
      this.$123list = const UndefinedWrapper
        .undefined()
,
    this.additionalProperties = const AdditionalProperties(),
    
  });

  static const $reflection = $ListReflection.instance;

  @override
  bool validate() {
    return super.validate();
  }


  Map<String, dynamic> toMap() {
    return _$$ListToMap(this);
  }
  factory $List.fromMap(Map<String, dynamic> src) {
    return _$$ListFromMap(src);
  }
  static $List? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return $List.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$$ListCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory $List.deserialize(Object? src) {
    return _$$ListDeserialize(src);
  }
  static $List? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return $List.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$$ListCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Object? serialize() {
    return _$$ListSerialize(this);
  }
}




