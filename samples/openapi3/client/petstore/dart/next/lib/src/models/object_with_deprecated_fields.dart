// Model def

import 'package:petstore_api/_internal.dart';


part 'object_with_deprecated_fields.reflection.dart';


/// ObjectWithDeprecatedFieldsMixin
///
/// Properties:
/// * [uuid] 
/// * [id] 
/// * [deprecatedRef] 
/// * [bars] 
mixin ObjectWithDeprecatedFieldsMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            String
> get uuid;
UndefinedWrapper<
            num
> get id;
UndefinedWrapper<
            DeprecatedObject
> get deprecatedRef;
UndefinedWrapper<
    List<
        
            String
>
> get bars;
  
}

/// ObjectWithDeprecatedFields
///
/// Properties:
/// * [uuid] 
/// * [id] 
/// * [deprecatedRef] 
/// * [bars] 
class ObjectWithDeprecatedFields with
$OpenApiObjectMixin,

ObjectWithDeprecatedFieldsMixin {
  @override
  UndefinedWrapper<
            String
> uuid;
  @override
  UndefinedWrapper<
            num
> id;
  @override
  UndefinedWrapper<
            DeprecatedObject
> deprecatedRef;
  @override
  UndefinedWrapper<
    List<
        
            String
>
> bars;

  AdditionalProperties<Object
?> additionalProperties;

  

  ObjectWithDeprecatedFields.$all({
        required this.uuid,
    required this.id,
    required this.deprecatedRef,
    required this.bars,
    required this.additionalProperties,
    
  });

  ObjectWithDeprecatedFields({
      this.uuid = const UndefinedWrapper
        .undefined()
,
  this.id = const UndefinedWrapper
        .undefined()
,
  this.deprecatedRef = const UndefinedWrapper
        .undefined()
,
  this.bars = const UndefinedWrapper
        .undefined()
,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = ObjectWithDeprecatedFieldsReflection.instance;
  ObjectWithDeprecatedFieldsReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory ObjectWithDeprecatedFields.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  ObjectWithDeprecatedFields clone() {
    return $reflection.clone(this);
  }
}
















