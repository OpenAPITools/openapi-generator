// Model def

import 'package:petstore_api/_internal.dart';


part 'variable.reflection.dart';
part 'variable.serialization.dart';


/// Value object
///
/// Properties:
/// * [name] 
/// * [value] 
mixin VariableMixin on
  $OpenApiObjectMixin {
  
            String
 get name;

            Value
 get value;
  
}

/// Value object
///
/// Properties:
/// * [name] 
/// * [value] 
class Variable with
$OpenApiObjectMixin,

VariableMixin {
  @override
  
            String
 name;
  @override
  
            Value
 value;

  AdditionalProperties<Object
?> additionalProperties;

  

  Variable.$all({
        required this.name,
    required this.value,
    required this.additionalProperties,
    
  });

  Variable({
    required  this.name     ,
required  this.value     ,
    this.additionalProperties = const AdditionalProperties(),
    
  });

  static const $reflection = VariableReflection.instance;
  VariableReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  Map<String, dynamic> toMap() {
    return _$VariableToMap(this);
  }
  factory Variable.fromMap(Map<String, dynamic> src) {
    return _$VariableFromMap(src);
  }
  static Variable? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return Variable.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$VariableCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory Variable.deserialize(Object? src) {
    return _$VariableDeserialize(src);
  }
  static Variable? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return Variable.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$VariableCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Map<String,dynamic> serialize() {
    return _$VariableSerialize(this);
  }
}




