// Model def

import 'package:petstore_api/_internal.dart';


part 'mixed_type_one_of_number.reflection.dart';


/// MixedTypeOneOfNumberMixin
mixin MixedTypeOneOfNumberMixin on
  $OpenApiObjectMixin {
    
  UndefinedWrapper<
            double
> get oneOf0;
  UndefinedWrapper<
            double
> get oneOf1;
}

/// MixedTypeOneOfNumber
class MixedTypeOneOfNumber with
$OpenApiObjectMixin,

MixedTypeOneOfNumberMixin {



  
  @override
  UndefinedWrapper<
            double
> oneOf0;
  
  @override
  UndefinedWrapper<
            double
> oneOf1;
  

  MixedTypeOneOfNumber.$all({
        
    
    required this.oneOf0,
    required this.oneOf1,
  });

  MixedTypeOneOfNumber({
        
    
    this.oneOf0 = const UndefinedWrapper.undefined(),
    this.oneOf1 = const UndefinedWrapper.undefined(),
  });

  static const $reflection = MixedTypeOneOfNumberReflection.instance;
  MixedTypeOneOfNumberReflection get $classReflection => $reflection;

  @override
  bool validate() {
      final oneOfs = [oneOf0,oneOf1,].where((e) => e.isDefined).take(2);
      if (oneOfs.length > 1) {
        // there must be AT MOST one "oneOf" schema.
        return false;
      }
      
    return super.validate();
  }

  factory MixedTypeOneOfNumber.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  MixedTypeOneOfNumber clone() {
    return $reflection.clone(this);
  }
}


