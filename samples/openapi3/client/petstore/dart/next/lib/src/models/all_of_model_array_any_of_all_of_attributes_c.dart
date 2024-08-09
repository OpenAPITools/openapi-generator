// Model def

import 'package:petstore_api/_internal.dart';


part 'all_of_model_array_any_of_all_of_attributes_c.reflection.dart';
part 'all_of_model_array_any_of_all_of_attributes_c.serialization.dart';


/// AllOfModelArrayAnyOfAllOfAttributesCMixin
///
/// Properties:
mixin AllOfModelArrayAnyOfAllOfAttributesCMixin on
  $OpenApiObjectMixin {
    
  UndefinedWrapper<
            Pet
> get oneOf0;
  UndefinedWrapper<
            Order
> get oneOf1;
}

/// AllOfModelArrayAnyOfAllOfAttributesC
///
/// Properties:
class AllOfModelArrayAnyOfAllOfAttributesC with
$OpenApiObjectMixin,

AllOfModelArrayAnyOfAllOfAttributesCMixin {

  AdditionalProperties<Object

?> additionalProperties;

  
  @override
  UndefinedWrapper<
            Pet
> oneOf0;
  
  @override
  UndefinedWrapper<
            Order
> oneOf1;
  

  AllOfModelArrayAnyOfAllOfAttributesC.$all({
        required this.additionalProperties,
    
    required this.oneOf0,
    required this.oneOf1,
  });

  AllOfModelArrayAnyOfAllOfAttributesC({
        this.additionalProperties = const AdditionalProperties(),
    
    this.oneOf0 = const UndefinedWrapper.undefined(),
    this.oneOf1 = const UndefinedWrapper.undefined(),
  });

  static const $reflection = AllOfModelArrayAnyOfAllOfAttributesCReflection.instance;
  AllOfModelArrayAnyOfAllOfAttributesCReflection get $classReflection => $reflection;

  @override
  bool validate() {
      final oneOfs = [oneOf0,oneOf1,].where((e) => e.isDefined).take(2);
      if (oneOfs.length > 1) {
        // there must be AT MOST one "oneOf" schema.
        return false;
      }
      
    return super.validate();
  }

  Map<String, dynamic> toMap() {
    return _$AllOfModelArrayAnyOfAllOfAttributesCToMap(this);
  }
  factory AllOfModelArrayAnyOfAllOfAttributesC.fromMap(Map<String, dynamic> src) {
    return _$AllOfModelArrayAnyOfAllOfAttributesCFromMap(src);
  }
  static AllOfModelArrayAnyOfAllOfAttributesC? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return AllOfModelArrayAnyOfAllOfAttributesC.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$AllOfModelArrayAnyOfAllOfAttributesCCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory AllOfModelArrayAnyOfAllOfAttributesC.deserialize(Object? src) {
    return _$AllOfModelArrayAnyOfAllOfAttributesCDeserialize(src);
  }
  static AllOfModelArrayAnyOfAllOfAttributesC? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return AllOfModelArrayAnyOfAllOfAttributesC.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$AllOfModelArrayAnyOfAllOfAttributesCCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Object? serialize() {
    return _$AllOfModelArrayAnyOfAllOfAttributesCSerialize(this);
  }
}




