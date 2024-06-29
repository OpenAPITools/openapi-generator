// Model def

import 'package:openapi/_internal.dart';


part 'apple.reflection.dart';
part 'apple.serialization.dart';


/// AppleMixin
///
/// Properties:
/// * [cultivar] 
/// * [origin] 
mixin AppleMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            String
> get cultivar;
UndefinedWrapper<
            String
> get origin;
  
}

/// Apple
///
/// Properties:
/// * [cultivar] 
/// * [origin] 
class Apple with
$OpenApiObjectMixin,


AppleMixin {
  @override
  UndefinedWrapper<
            String
> cultivar;
  @override
  UndefinedWrapper<
            String
> origin;

  AdditionalProperties<Object
?> additionalProperties;

  

  Apple.$all({
        required this.cultivar,
    required this.origin,
    required this.additionalProperties,
    
  });

  Apple({
      this.cultivar = const UndefinedWrapper
        .undefined()
,
  this.origin = const UndefinedWrapper
        .undefined()
,
    this.additionalProperties = const AdditionalProperties(),
    
  });

  static const $reflection = AppleReflection.instance;

  @override
  bool validate() {
    return super.validate();
  }


  Map<String, dynamic> toMap() {
    return _$AppleToMap(this);
  }
  factory Apple.fromMap(Map<String, dynamic> src) {
    return _$AppleFromMap(src);
  }
  static Apple? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return Apple.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$AppleCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory Apple.deserialize(Object? src) {
    return _$AppleDeserialize(src);
  }
  static Apple? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return Apple.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$AppleCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Map<String,dynamic> serialize() {
    return _$AppleSerialize(this);
  }
}




