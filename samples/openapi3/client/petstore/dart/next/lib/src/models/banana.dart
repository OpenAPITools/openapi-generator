// Model def

import 'package:petstore_api/_internal.dart';


part 'banana.reflection.dart';
part 'banana.serialization.dart';


/// BananaMixin
///
/// Properties:
/// * [lengthCm] 
mixin BananaMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            num
> get lengthCm;
  
}

/// Banana
///
/// Properties:
/// * [lengthCm] 
class Banana with
$OpenApiObjectMixin,

BananaMixin {
  @override
  UndefinedWrapper<
            num
> lengthCm;

  AdditionalProperties<Object
?> additionalProperties;

  

  Banana.$all({
        required this.lengthCm,
    required this.additionalProperties,
    
  });

  Banana({
      this.lengthCm = const UndefinedWrapper
        .undefined()
,
    this.additionalProperties = const AdditionalProperties(),
    
  });

  static const $reflection = BananaReflection.instance;
  BananaReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  Map<String, dynamic> toMap() {
    return _$BananaToMap(this);
  }
  factory Banana.fromMap(Map<String, dynamic> src) {
    return _$BananaFromMap(src);
  }
  static Banana? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return Banana.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$BananaCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory Banana.deserialize(Object? src) {
    return _$BananaDeserialize(src);
  }
  static Banana? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return Banana.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$BananaCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Map<String,dynamic> serialize() {
    return _$BananaSerialize(this);
  }
}




