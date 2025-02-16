// Model def

import 'package:petstore_api/_internal.dart';


part 'pig.reflection.dart';


/// PigMixin
///
/// Properties:
/// * [className] 
mixin PigMixin on
  $OpenApiObjectMixin {
  
            String
 get className;
  
  UndefinedWrapper<
            BasquePig
> get oneOf0;
  UndefinedWrapper<
            DanishPig
> get oneOf1;
}

/// Pig
///
/// Properties:
/// * [className] 
class Pig with
$OpenApiObjectMixin,

PigMixin {
  @override
  
            String
 className;

  AdditionalProperties<Object
?> additionalProperties;

  
  @override
  UndefinedWrapper<
            BasquePig
> oneOf0;
  
  @override
  UndefinedWrapper<
            DanishPig
> oneOf1;
  

  Pig.$all({
        required this.className,
    required this.additionalProperties,
    
    required this.oneOf0,
    required this.oneOf1,
  });

  Pig({
    required  this.className     ,
    AdditionalProperties<Object
?>? additionalProperties,
    
    this.oneOf0 = const UndefinedWrapper.undefined(),
    this.oneOf1 = const UndefinedWrapper.undefined(),
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = PigReflection.instance;
  PigReflection get $classReflection => $reflection;

  @override
  bool validate() {
      final oneOfs = [oneOf0,oneOf1,].where((e) => e.isDefined).take(2);
      if (oneOfs.length > 1) {
        // there must be AT MOST one "oneOf" schema.
        return false;
      }
      
    return super.validate();
  }

  factory Pig.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  Pig clone() {
    return $reflection.clone(this);
  }
}





