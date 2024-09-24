// Model def

import 'package:petstore_api/_internal.dart';


part 'mixed_properties_and_additional_properties_class.reflection.dart';


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
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = MixedPropertiesAndAdditionalPropertiesClassReflection.instance;
  MixedPropertiesAndAdditionalPropertiesClassReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory MixedPropertiesAndAdditionalPropertiesClass.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  MixedPropertiesAndAdditionalPropertiesClass clone() {
    return $reflection.clone(this);
  }
}













