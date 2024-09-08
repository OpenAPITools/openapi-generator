// Model def

import 'package:petstore_api/_internal.dart';


part 'quadrilateral_interface.reflection.dart';


/// QuadrilateralInterfaceMixin
///
/// Properties:
/// * [quadrilateralType] 
mixin QuadrilateralInterfaceMixin on
  $OpenApiObjectMixin {
  
            String
 get quadrilateralType;
  
}

/// QuadrilateralInterface
///
/// Properties:
/// * [quadrilateralType] 
class QuadrilateralInterface with
$OpenApiObjectMixin,

QuadrilateralInterfaceMixin {
  @override
  
            String
 quadrilateralType;

  AdditionalProperties<Object
?> additionalProperties;

  

  QuadrilateralInterface.$all({
        required this.quadrilateralType,
    required this.additionalProperties,
    
  });

  QuadrilateralInterface({
    required  this.quadrilateralType     ,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = QuadrilateralInterfaceReflection.instance;
  QuadrilateralInterfaceReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory QuadrilateralInterface.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  QuadrilateralInterface clone() {
    return $reflection.clone(this);
  }
}





