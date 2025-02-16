// Model def

import 'package:petstore_api/_internal.dart';


part 'number_only.reflection.dart';


/// NumberOnlyMixin
///
/// Properties:
/// * [justNumber] 
mixin NumberOnlyMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            num
> get justNumber;
  
}

/// NumberOnly
///
/// Properties:
/// * [justNumber] 
class NumberOnly with
$OpenApiObjectMixin,

NumberOnlyMixin {
  @override
  UndefinedWrapper<
            num
> justNumber;

  AdditionalProperties<Object
?> additionalProperties;

  

  NumberOnly.$all({
        required this.justNumber,
    required this.additionalProperties,
    
  });

  NumberOnly({
      this.justNumber = const UndefinedWrapper
        .undefined()
,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = NumberOnlyReflection.instance;
  NumberOnlyReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory NumberOnly.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  NumberOnly clone() {
    return $reflection.clone(this);
  }
}





