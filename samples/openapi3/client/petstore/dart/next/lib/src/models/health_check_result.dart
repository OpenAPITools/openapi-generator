// Model def

import 'package:openapi/_internal.dart';


part 'health_check_result.reflection.dart';
part 'health_check_result.serialization.dart';


/// Just a string to inform instance is up and running. Make it nullable in hope to get it as pointer in generated model.
///
/// Properties:
/// * [nullableMessage] 
mixin HealthCheckResultMixin on 
  $OpenApiObjectMixin {
  UndefinedWrapper<
            String
?> get nullableMessage;
  
}

/// Just a string to inform instance is up and running. Make it nullable in hope to get it as pointer in generated model.
///
/// Properties:
/// * [nullableMessage] 
class HealthCheckResult with
$OpenApiObjectMixin,


HealthCheckResultMixin {
  @override
  UndefinedWrapper<
            String
?> nullableMessage;

  

  

  HealthCheckResult.$all({
        required this.nullableMessage,
    
    
  });

  HealthCheckResult({
      this.nullableMessage = const UndefinedWrapper
        .undefined()
,
    
    
  });

  static const $reflection = HealthCheckResultReflection.instance;

  @override
  bool validate() {
    return super.validate();
  }


  Map<String, dynamic> toMap() {
    return _$HealthCheckResultToMap(this);
  }
  factory HealthCheckResult.fromMap(Map<String, dynamic> src) {
    return _$HealthCheckResultFromMap(src);
  }
  static HealthCheckResult? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return HealthCheckResult.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$HealthCheckResultCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory HealthCheckResult.deserialize(Object? src) {
    return _$HealthCheckResultDeserialize(src);
  }
  static HealthCheckResult? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return HealthCheckResult.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$HealthCheckResultCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Object? serialize() {
    return _$HealthCheckResultSerialize(this);
  }
}




