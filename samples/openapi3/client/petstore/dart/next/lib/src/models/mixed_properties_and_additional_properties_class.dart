// Model def

import 'package:openapi/_internal.dart';


part 'mixed_properties_and_additional_properties_class.reflection.dart';
part 'mixed_properties_and_additional_properties_class.serialization.dart';


/// MixedPropertiesAndAdditionalPropertiesClassMixin
///
/// Properties:
/// * [uuid] 
/// * [dateTime] 
/// * [map] 
mixin MixedPropertiesAndAdditionalPropertiesClassMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            String
> get uuid;
UndefinedWrapper<
            DateTime
> get dateTime;
UndefinedWrapper<
    Map<String, 
        
            Animal
>
> get map;
  
}

/// MixedPropertiesAndAdditionalPropertiesClass
///
/// Properties:
/// * [uuid] 
/// * [dateTime] 
/// * [map] 
class MixedPropertiesAndAdditionalPropertiesClass with
$OpenApiObjectMixin,


MixedPropertiesAndAdditionalPropertiesClassMixin {
  @override
  UndefinedWrapper<
            String
> uuid;
  @override
  UndefinedWrapper<
            DateTime
> dateTime;
  @override
  UndefinedWrapper<
    Map<String, 
        
            Animal
>
> map;

  AdditionalProperties<Object
?> additionalProperties;

  

  MixedPropertiesAndAdditionalPropertiesClass.$all({
        required this.uuid,
    required this.dateTime,
    required this.map,
    required this.additionalProperties,
    
  });

  MixedPropertiesAndAdditionalPropertiesClass({
      this.uuid = const UndefinedWrapper
        .undefined()
,
  this.dateTime = const UndefinedWrapper
        .undefined()
,
  this.map = const UndefinedWrapper
        .undefined()
,
    this.additionalProperties = const AdditionalProperties(),
    
  });

  static const $reflection = MixedPropertiesAndAdditionalPropertiesClassReflection.instance;

  @override
  bool validate() {
    return super.validate();
  }


  Map<String, dynamic> toMap() {
    return _$MixedPropertiesAndAdditionalPropertiesClassToMap(this);
  }
  factory MixedPropertiesAndAdditionalPropertiesClass.fromMap(Map<String, dynamic> src) {
    return _$MixedPropertiesAndAdditionalPropertiesClassFromMap(src);
  }
  static MixedPropertiesAndAdditionalPropertiesClass? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return MixedPropertiesAndAdditionalPropertiesClass.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$MixedPropertiesAndAdditionalPropertiesClassCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory MixedPropertiesAndAdditionalPropertiesClass.deserialize(Object? src) {
    return _$MixedPropertiesAndAdditionalPropertiesClassDeserialize(src);
  }
  static MixedPropertiesAndAdditionalPropertiesClass? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return MixedPropertiesAndAdditionalPropertiesClass.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$MixedPropertiesAndAdditionalPropertiesClassCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Object? serialize() {
    return _$MixedPropertiesAndAdditionalPropertiesClassSerialize(this);
  }
}




