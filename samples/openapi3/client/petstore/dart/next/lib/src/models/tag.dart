// Model def

import 'package:petstore_api/_internal.dart';


part 'tag.reflection.dart';


/// TagMixin
///
/// Properties:
/// * [id] 
/// * [name] 
mixin TagMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            int
> get id;
UndefinedWrapper<
            String
> get name;
  
}

/// Tag
///
/// Properties:
/// * [id] 
/// * [name] 
class Tag with
$OpenApiObjectMixin,

TagMixin {
  @override
  UndefinedWrapper<
            int
> id;
  @override
  UndefinedWrapper<
            String
> name;

  AdditionalProperties<Object
?> additionalProperties;

  

  Tag.$all({
        required this.id,
    required this.name,
    required this.additionalProperties,
    
  });

  Tag({
      this.id = const UndefinedWrapper
        .undefined()
,
  this.name = const UndefinedWrapper
        .undefined()
,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = TagReflection.instance;
  TagReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory Tag.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  Tag clone() {
    return $reflection.clone(this);
  }
}








