// Model def

import 'package:petstore_api/_internal.dart';


part 'all_of_model_array_any_of_all_of_attributes.reflection.dart';
part 'all_of_model_array_any_of_all_of_attributes.serialization.dart';


/// AllOfModelArrayAnyOfAllOfAttributesMixin
///
/// Properties:
/// * [C] 
mixin AllOfModelArrayAnyOfAllOfAttributesMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            AllOfModelArrayAnyOfAllOfAttributesC
> get C;
  
}

/// AllOfModelArrayAnyOfAllOfAttributes
///
/// Properties:
/// * [C] 
class AllOfModelArrayAnyOfAllOfAttributes with
$OpenApiObjectMixin,

AllOfModelArrayAnyOfAllOfAttributesMixin {
  @override
  UndefinedWrapper<
            AllOfModelArrayAnyOfAllOfAttributesC
> C;

  AdditionalProperties<Object
?> additionalProperties;

  

  AllOfModelArrayAnyOfAllOfAttributes.$all({
        required this.C,
    required this.additionalProperties,
    
  });

  AllOfModelArrayAnyOfAllOfAttributes({
      this.C = const UndefinedWrapper
        .undefined()
,
    this.additionalProperties = const AdditionalProperties(),
    
  });

  static const $reflection = AllOfModelArrayAnyOfAllOfAttributesReflection.instance;
  AllOfModelArrayAnyOfAllOfAttributesReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  Map<String, dynamic> toMap() {
    return _$AllOfModelArrayAnyOfAllOfAttributesToMap(this);
  }
  factory AllOfModelArrayAnyOfAllOfAttributes.fromMap(Map<String, dynamic> src) {
    return _$AllOfModelArrayAnyOfAllOfAttributesFromMap(src);
  }
  static AllOfModelArrayAnyOfAllOfAttributes? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return AllOfModelArrayAnyOfAllOfAttributes.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$AllOfModelArrayAnyOfAllOfAttributesCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory AllOfModelArrayAnyOfAllOfAttributes.deserialize(Object? src) {
    return _$AllOfModelArrayAnyOfAllOfAttributesDeserialize(src);
  }
  static AllOfModelArrayAnyOfAllOfAttributes? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return AllOfModelArrayAnyOfAllOfAttributes.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$AllOfModelArrayAnyOfAllOfAttributesCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Map<String,dynamic> serialize() {
    return _$AllOfModelArrayAnyOfAllOfAttributesSerialize(this);
  }
}




