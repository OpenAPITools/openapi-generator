// Model def

import 'package:openapi/_internal.dart';


part 'animal.reflection.dart';
part 'animal.serialization.dart';


/// AnimalMixin
///
/// Properties:
/// * [className] 
/// * [color] 
mixin AnimalMixin on 
  $OpenApiObjectMixin {
  
            String
 get className;
UndefinedWrapper<
            String
> get color;
  
}

/// Animal
///
/// Properties:
/// * [className] 
/// * [color] 
class Animal with
$OpenApiObjectMixin,


AnimalMixin {
  @override
  
            String
 className;
  @override
  UndefinedWrapper<
            String
> color;

  

  

  Animal.$all({
        required this.className,
    required this.color,
    
    
  });

  Animal({
    required  this.className     ,
  this.color = const UndefinedWrapper
    (
        
        'red'
    )
    
,
    
    
  });

  static const $reflection = AnimalReflection.instance;

  @override
  bool validate() {
    return super.validate();
  }


  Map<String, dynamic> toMap() {
    return _$AnimalToMap(this);
  }
  factory Animal.fromMap(Map<String, dynamic> src) {
    return _$AnimalFromMap(src);
  }
  static Animal? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return Animal.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$AnimalCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory Animal.deserialize(Object? src) {
    return _$AnimalDeserialize(src);
  }
  static Animal? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return Animal.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$AnimalCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Object? serialize() {
    return _$AnimalSerialize(this);
  }
}




