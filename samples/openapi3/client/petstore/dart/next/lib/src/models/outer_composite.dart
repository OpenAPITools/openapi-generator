// Model def

import 'package:petstore_api/_internal.dart';


part 'outer_composite.reflection.dart';


/// OuterCompositeMixin
///
/// Properties:
/// * [myNumber] 
/// * [myString] 
/// * [myBoolean] 
mixin OuterCompositeMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            num
> get myNumber;
UndefinedWrapper<
            String
> get myString;
UndefinedWrapper<
            bool
> get myBoolean;
  
}

/// OuterComposite
///
/// Properties:
/// * [myNumber] 
/// * [myString] 
/// * [myBoolean] 
class OuterComposite with
$OpenApiObjectMixin,

OuterCompositeMixin {
  @override
  UndefinedWrapper<
            num
> myNumber;
  @override
  UndefinedWrapper<
            String
> myString;
  @override
  UndefinedWrapper<
            bool
> myBoolean;

  AdditionalProperties<Object
?> additionalProperties;

  

  OuterComposite.$all({
        required this.myNumber,
    required this.myString,
    required this.myBoolean,
    required this.additionalProperties,
    
  });

  OuterComposite({
      this.myNumber = const UndefinedWrapper
        .undefined()
,
  this.myString = const UndefinedWrapper
        .undefined()
,
  this.myBoolean = const UndefinedWrapper
        .undefined()
,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = OuterCompositeReflection.instance;
  OuterCompositeReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory OuterComposite.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  OuterComposite clone() {
    return $reflection.clone(this);
  }
}











