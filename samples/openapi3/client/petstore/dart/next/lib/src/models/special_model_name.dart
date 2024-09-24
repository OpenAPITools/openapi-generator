// Model def

import 'package:petstore_api/_internal.dart';


part 'special_model_name.reflection.dart';


/// SpecialModelNameMixin
///
/// Properties:
/// * [$specialPropertyName] 
/// * [specialModelName] 
mixin SpecialModelNameMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            int
> get $specialPropertyName;
UndefinedWrapper<
            String
> get specialModelName;
  
}

/// SpecialModelName
///
/// Properties:
/// * [$specialPropertyName] 
/// * [specialModelName] 
class SpecialModelName with
$OpenApiObjectMixin,

SpecialModelNameMixin {
  @override
  UndefinedWrapper<
            int
> $specialPropertyName;
  @override
  UndefinedWrapper<
            String
> specialModelName;

  AdditionalProperties<Object
?> additionalProperties;

  

  SpecialModelName.$all({
        required this.$specialPropertyName,
    required this.specialModelName,
    required this.additionalProperties,
    
  });

  SpecialModelName({
      this.$specialPropertyName = const UndefinedWrapper
        .undefined()
,
  this.specialModelName = const UndefinedWrapper
        .undefined()
,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = SpecialModelNameReflection.instance;
  SpecialModelNameReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory SpecialModelName.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  SpecialModelName clone() {
    return $reflection.clone(this);
  }
}








