// Model def

import 'package:petstore_api/_internal.dart';


part 'quadrilateral_interface.reflection.dart';
part 'quadrilateral_interface.serialization.dart';


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
    this.additionalProperties = const AdditionalProperties(),
    
  });

  static const $reflection = QuadrilateralInterfaceReflection.instance;
  QuadrilateralInterfaceReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  Map<String, dynamic> toMap() {
    return _$QuadrilateralInterfaceToMap(this);
  }
  factory QuadrilateralInterface.fromMap(Map<String, dynamic> src) {
    return _$QuadrilateralInterfaceFromMap(src);
  }
  static QuadrilateralInterface? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return QuadrilateralInterface.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$QuadrilateralInterfaceCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory QuadrilateralInterface.deserialize(Object? src) {
    return _$QuadrilateralInterfaceDeserialize(src);
  }
  static QuadrilateralInterface? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return QuadrilateralInterface.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$QuadrilateralInterfaceCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Map<String,dynamic> serialize() {
    return _$QuadrilateralInterfaceSerialize(this);
  }
}




