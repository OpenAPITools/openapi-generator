// Model def

import 'package:petstore_api/_internal.dart';


part 'dog.reflection.dart';
part 'dog.serialization.dart';


/// DogMixin
///
/// Properties:
/// * [breed] 
mixin DogMixin on
  AnimalMixin, $OpenApiObjectMixin {
  UndefinedWrapper<
            String
> get breed;
  
}

/// Dog
///
/// Properties:
/// * [color] 
/// * [breed] 
/// * [className] 
class Dog with
$OpenApiObjectMixin,
AnimalMixin,
DogMixin {
  @override
  UndefinedWrapper<
            String
> color;
  @override
  UndefinedWrapper<
            String
> breed;
  @override
  
            String
 className;

  AdditionalProperties<Object
?> additionalProperties;

  

  Dog.$all({
        required this.color,
    required this.breed,
    required this.className,
    required this.additionalProperties,
    
  });

  Dog({
      this.color = const UndefinedWrapper
    (
        
        'red'
    )
    
,
  this.breed = const UndefinedWrapper
        .undefined()
,
required  this.className     ,
    this.additionalProperties = const AdditionalProperties(),
    
  });

  static const $reflection = DogReflection.instance;
  DogReflection get $classReflection => $reflection;

  @override
  bool validate() {
      
      
    return super.validate();
  }

  Map<String, dynamic> toMap() {
    return _$DogToMap(this);
  }
  factory Dog.fromMap(Map<String, dynamic> src) {
    return _$DogFromMap(src);
  }
  static Dog? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return Dog.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$DogCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory Dog.deserialize(Object? src) {
    return _$DogDeserialize(src);
  }
  static Dog? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return Dog.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$DogCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Map<String,dynamic> serialize() {
    return _$DogSerialize(this);
  }
}




