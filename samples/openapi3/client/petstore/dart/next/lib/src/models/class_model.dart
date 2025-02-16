// Model def

import 'package:petstore_api/_internal.dart';


part 'class_model.reflection.dart';


/// Model for testing model with \"_class\" property
///
/// Properties:
/// * [propertyClass] 
mixin ClassModelMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            String
> get propertyClass;
  
}

/// Model for testing model with \"_class\" property
///
/// Properties:
/// * [propertyClass] 
class ClassModel with
$OpenApiObjectMixin,

ClassModelMixin {
  @override
  UndefinedWrapper<
            String
> propertyClass;

  AdditionalProperties<Object
?> additionalProperties;

  

  ClassModel.$all({
        required this.propertyClass,
    required this.additionalProperties,
    
  });

  ClassModel({
      this.propertyClass = const UndefinedWrapper
        .undefined()
,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = ClassModelReflection.instance;
  ClassModelReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory ClassModel.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  ClassModel clone() {
    return $reflection.clone(this);
  }
}





