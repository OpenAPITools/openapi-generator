// Model def

import 'package:openapi/_internal.dart';


part 'model_with_one_of_any_of_properties.reflection.dart';
part 'model_with_one_of_any_of_properties.serialization.dart';


/// ModelWithOneOfAnyOfPropertiesMixin
///
/// Properties:
/// * [oneofProp] 
/// * [anyofProp] 
mixin ModelWithOneOfAnyOfPropertiesMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            ArrayOneOf
> get oneofProp;
UndefinedWrapper<
            ArrayAnyOf
> get anyofProp;
  
}

/// ModelWithOneOfAnyOfProperties
///
/// Properties:
/// * [oneofProp] 
/// * [anyofProp] 
class ModelWithOneOfAnyOfProperties with
$OpenApiObjectMixin,


ModelWithOneOfAnyOfPropertiesMixin {
  @override
  UndefinedWrapper<
            ArrayOneOf
> oneofProp;
  @override
  UndefinedWrapper<
            ArrayAnyOf
> anyofProp;

  AdditionalProperties<Object
?> additionalProperties;

  

  ModelWithOneOfAnyOfProperties.$all({
        required this.oneofProp,
    required this.anyofProp,
    required this.additionalProperties,
    
  });

  ModelWithOneOfAnyOfProperties({
      this.oneofProp = const UndefinedWrapper
        .undefined()
,
  this.anyofProp = const UndefinedWrapper
        .undefined()
,
    this.additionalProperties = const AdditionalProperties(),
    
  });

  static const $reflection = ModelWithOneOfAnyOfPropertiesReflection.instance;

  @override
  bool validate() {
    return super.validate();
  }


  Map<String, dynamic> toMap() {
    return _$ModelWithOneOfAnyOfPropertiesToMap(this);
  }
  factory ModelWithOneOfAnyOfProperties.fromMap(Map<String, dynamic> src) {
    return _$ModelWithOneOfAnyOfPropertiesFromMap(src);
  }
  static ModelWithOneOfAnyOfProperties? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return ModelWithOneOfAnyOfProperties.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$ModelWithOneOfAnyOfPropertiesCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory ModelWithOneOfAnyOfProperties.deserialize(Object? src) {
    return _$ModelWithOneOfAnyOfPropertiesDeserialize(src);
  }
  static ModelWithOneOfAnyOfProperties? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return ModelWithOneOfAnyOfProperties.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$ModelWithOneOfAnyOfPropertiesCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Map<String,dynamic> serialize() {
    return _$ModelWithOneOfAnyOfPropertiesSerialize(this);
  }
}




