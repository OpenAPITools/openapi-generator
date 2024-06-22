// Model def

import 'package:openapi/_internal.dart';


part 'whale.reflection.dart';
part 'whale.serialization.dart';


/// WhaleMixin
///
/// Properties:
/// * [hasBaleen] 
/// * [hasTeeth] 
/// * [className] 
mixin WhaleMixin on 
  $OpenApiObjectMixin {
  UndefinedWrapper<
            bool
> get hasBaleen;
UndefinedWrapper<
            bool
> get hasTeeth;

            String
 get className;
  
}

/// Whale
///
/// Properties:
/// * [hasBaleen] 
/// * [hasTeeth] 
/// * [className] 
class Whale with
$OpenApiObjectMixin,


WhaleMixin {
  @override
  UndefinedWrapper<
            bool
> hasBaleen;
  @override
  UndefinedWrapper<
            bool
> hasTeeth;
  @override
  
            String
 className;

  

  

  Whale.$all({
        required this.hasBaleen,
    required this.hasTeeth,
    required this.className,
    
    
  });

  Whale({
      this.hasBaleen = const UndefinedWrapper
        .undefined()
,
  this.hasTeeth = const UndefinedWrapper
        .undefined()
,
required  this.className     ,
    
    
  });

  static const $reflection = WhaleReflection.instance;

  @override
  bool validate() {
    return super.validate();
  }


  Map<String, dynamic> toMap() {
    return _$WhaleToMap(this);
  }
  factory Whale.fromMap(Map<String, dynamic> src) {
    return _$WhaleFromMap(src);
  }
  static Whale? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return Whale.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$WhaleCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory Whale.deserialize(Object? src) {
    return _$WhaleDeserialize(src);
  }
  static Whale? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return Whale.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$WhaleCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Object? serialize() {
    return _$WhaleSerialize(this);
  }
}




