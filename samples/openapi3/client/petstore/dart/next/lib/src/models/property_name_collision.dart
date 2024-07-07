// Model def

import 'package:petstore_api/_internal.dart';


part 'property_name_collision.reflection.dart';
part 'property_name_collision.serialization.dart';


/// PropertyNameCollisionMixin
///
/// Properties:
/// * [$type] 
/// * [type] 
/// * [type$] 
mixin PropertyNameCollisionMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            String
> get $type;
UndefinedWrapper<
            String
> get type;
UndefinedWrapper<
            String
> get type$;
  
}

/// PropertyNameCollision
///
/// Properties:
/// * [$type] 
/// * [type] 
/// * [type$] 
class PropertyNameCollision with
$OpenApiObjectMixin,

PropertyNameCollisionMixin {
  @override
  UndefinedWrapper<
            String
> $type;
  @override
  UndefinedWrapper<
            String
> type;
  @override
  UndefinedWrapper<
            String
> type$;

  AdditionalProperties<Object
?> additionalProperties;

  

  PropertyNameCollision.$all({
        required this.$type,
    required this.type,
    required this.type$,
    required this.additionalProperties,
    
  });

  PropertyNameCollision({
      this.$type = const UndefinedWrapper
        .undefined()
,
  this.type = const UndefinedWrapper
        .undefined()
,
  this.type$ = const UndefinedWrapper
        .undefined()
,
    this.additionalProperties = const AdditionalProperties(),
    
  });

  static const $reflection = PropertyNameCollisionReflection.instance;
  PropertyNameCollisionReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  Map<String, dynamic> toMap() {
    return _$PropertyNameCollisionToMap(this);
  }
  factory PropertyNameCollision.fromMap(Map<String, dynamic> src) {
    return _$PropertyNameCollisionFromMap(src);
  }
  static PropertyNameCollision? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return PropertyNameCollision.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$PropertyNameCollisionCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory PropertyNameCollision.deserialize(Object? src) {
    return _$PropertyNameCollisionDeserialize(src);
  }
  static PropertyNameCollision? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return PropertyNameCollision.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$PropertyNameCollisionCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Map<String,dynamic> serialize() {
    return _$PropertyNameCollisionSerialize(this);
  }
}




