// Model def

import 'package:openapi/_internal.dart';


part 'special_model_name.reflection.dart';
part 'special_model_name.serialization.dart';


/// SpecialModelNameMixin
///
/// Properties:
/// * [$specialPropertyName] 
/// * [specialModelName] 
mixin SpecialModelNameMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            int
> get $specialPropertyName;
UndefinedWrapper<
            String
> get specialModelName;
  
}

/// SpecialModelName
///
/// Properties:
/// * [$specialPropertyName] 
/// * [specialModelName] 
class SpecialModelName with
$OpenApiObjectMixin,


SpecialModelNameMixin {
  @override
  UndefinedWrapper<
            int
> $specialPropertyName;
  @override
  UndefinedWrapper<
            String
> specialModelName;

  AdditionalProperties<Object
?> additionalProperties;

  

  SpecialModelName.$all({
        required this.$specialPropertyName,
    required this.specialModelName,
    required this.additionalProperties,
    
  });

  SpecialModelName({
      this.$specialPropertyName = const UndefinedWrapper
        .undefined()
,
  this.specialModelName = const UndefinedWrapper
        .undefined()
,
    this.additionalProperties = const AdditionalProperties(),
    
  });

  static const $reflection = SpecialModelNameReflection.instance;

  @override
  bool validate() {
    return super.validate();
  }


  Map<String, dynamic> toMap() {
    return _$SpecialModelNameToMap(this);
  }
  factory SpecialModelName.fromMap(Map<String, dynamic> src) {
    return _$SpecialModelNameFromMap(src);
  }
  static SpecialModelName? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return SpecialModelName.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$SpecialModelNameCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory SpecialModelName.deserialize(Object? src) {
    return _$SpecialModelNameDeserialize(src);
  }
  static SpecialModelName? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return SpecialModelName.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$SpecialModelNameCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Map<String,dynamic> serialize() {
    return _$SpecialModelNameSerialize(this);
  }
}




