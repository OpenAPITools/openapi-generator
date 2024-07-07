// Model def

import 'package:petstore_api/_internal.dart';


part 'name.reflection.dart';
part 'name.serialization.dart';


/// Model for testing model name same as property name
///
/// Properties:
/// * [name] 
/// * [snakeCase] 
/// * [property] 
/// * [$123number] 
mixin NameMixin on
  $OpenApiObjectMixin {
  
            int
 get name;
UndefinedWrapper<
            int
> get snakeCase;
UndefinedWrapper<
            String
> get property;
UndefinedWrapper<
            int
> get $123number;
  
}

/// Model for testing model name same as property name
///
/// Properties:
/// * [name] 
/// * [snakeCase] 
/// * [property] 
/// * [$123number] 
class Name with
$OpenApiObjectMixin,

NameMixin {
  @override
  
            int
 name;
  @override
  UndefinedWrapper<
            int
> snakeCase;
  @override
  UndefinedWrapper<
            String
> property;
  @override
  UndefinedWrapper<
            int
> $123number;

  AdditionalProperties<Object
?> additionalProperties;

  

  Name.$all({
        required this.name,
    required this.snakeCase,
    required this.property,
    required this.$123number,
    required this.additionalProperties,
    
  });

  Name({
    required  this.name     ,
  this.snakeCase = const UndefinedWrapper
        .undefined()
,
  this.property = const UndefinedWrapper
        .undefined()
,
  this.$123number = const UndefinedWrapper
        .undefined()
,
    this.additionalProperties = const AdditionalProperties(),
    
  });

  static const $reflection = NameReflection.instance;
  NameReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  Map<String, dynamic> toMap() {
    return _$NameToMap(this);
  }
  factory Name.fromMap(Map<String, dynamic> src) {
    return _$NameFromMap(src);
  }
  static Name? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return Name.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$NameCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory Name.deserialize(Object? src) {
    return _$NameDeserialize(src);
  }
  static Name? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return Name.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$NameCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Map<String,dynamic> serialize() {
    return _$NameSerialize(this);
  }
}




