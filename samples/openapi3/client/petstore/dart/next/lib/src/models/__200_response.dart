// Model def

import 'package:openapi/_internal.dart';


part '__200_response.reflection.dart';
part '__200_response.serialization.dart';


/// Model for testing model name starting with number
///
/// Properties:
/// * [name] 
/// * [propertyClass] 
mixin $200ResponseMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            int
> get name;
UndefinedWrapper<
            String
> get propertyClass;
  
}

/// Model for testing model name starting with number
///
/// Properties:
/// * [name] 
/// * [propertyClass] 
class $200Response with
$OpenApiObjectMixin,


$200ResponseMixin {
  @override
  UndefinedWrapper<
            int
> name;
  @override
  UndefinedWrapper<
            String
> propertyClass;

  AdditionalProperties<Object
?> additionalProperties;

  

  $200Response.$all({
        required this.name,
    required this.propertyClass,
    required this.additionalProperties,
    
  });

  $200Response({
      this.name = const UndefinedWrapper
        .undefined()
,
  this.propertyClass = const UndefinedWrapper
        .undefined()
,
    this.additionalProperties = const AdditionalProperties(),
    
  });

  static const $reflection = $200ResponseReflection.instance;

  @override
  bool validate() {
    return super.validate();
  }


  Map<String, dynamic> toMap() {
    return _$$200ResponseToMap(this);
  }
  factory $200Response.fromMap(Map<String, dynamic> src) {
    return _$$200ResponseFromMap(src);
  }
  static $200Response? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return $200Response.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$$200ResponseCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory $200Response.deserialize(Object? src) {
    return _$$200ResponseDeserialize(src);
  }
  static $200Response? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return $200Response.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$$200ResponseCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Map<String,dynamic> serialize() {
    return _$$200ResponseSerialize(this);
  }
}




