// Model def

import 'package:openapi/_internal.dart';


part 'scalar_any_of.reflection.dart';
part 'scalar_any_of.serialization.dart';


/// Values of scalar type using anyOf
mixin ScalarAnyOfMixin on
  $OpenApiObjectMixin {
    
  UndefinedWrapper<
            String
> get anyOf0;
  UndefinedWrapper<
            num
> get anyOf1;
  UndefinedWrapper<
            bool
> get anyOf2;
}

/// Values of scalar type using anyOf
class ScalarAnyOf with
$OpenApiObjectMixin,


ScalarAnyOfMixin {

  AdditionalProperties<Object
?> additionalProperties;

  
  @override
  UndefinedWrapper<
            String
> anyOf0;
  
  @override
  UndefinedWrapper<
            num
> anyOf1;
  
  @override
  UndefinedWrapper<
            bool
> anyOf2;
  

  ScalarAnyOf.$all({
        required this.additionalProperties,
    
    required this.anyOf0,
    required this.anyOf1,
    required this.anyOf2,
  });

  ScalarAnyOf({
        this.additionalProperties = const AdditionalProperties(),
    
    this.anyOf0 = const UndefinedWrapper.undefined(),
    
    this.anyOf1 = const UndefinedWrapper.undefined(),
    
    this.anyOf2 = const UndefinedWrapper.undefined(),
    
  });

  static const $reflection = ScalarAnyOfReflection.instance;

  @override
  bool validate() {
      
      final anyOfs = [anyOf0,anyOf1,anyOf2,].where((e) => e.isDefined).take(1);
      if (anyOfs.length == 0) {
        // there must be AT LEAST one "anyOf" schema.
        return false;
      }
      
    return super.validate();
  }


  Map<String, dynamic> toMap() {
    return _$ScalarAnyOfToMap(this);
  }
  factory ScalarAnyOf.fromMap(Map<String, dynamic> src) {
    return _$ScalarAnyOfFromMap(src);
  }
  static ScalarAnyOf? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return ScalarAnyOf.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$ScalarAnyOfCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory ScalarAnyOf.deserialize(Object? src) {
    return _$ScalarAnyOfDeserialize(src);
  }
  static ScalarAnyOf? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return ScalarAnyOf.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$ScalarAnyOfCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Object? serialize() {
    return _$ScalarAnyOfSerialize(this);
  }
}




