// Model def

import 'package:petstore_api/_internal.dart';


part 'array_any_of.reflection.dart';
part 'array_any_of.serialization.dart';


/// ArrayAnyOfMixin
mixin ArrayAnyOfMixin on
  $OpenApiObjectMixin {
    
  UndefinedWrapper<
            int
> get anyOf0;
  UndefinedWrapper<
    List<
        
            String
>
> get anyOf1;
}

/// ArrayAnyOf
class ArrayAnyOf with
$OpenApiObjectMixin,

ArrayAnyOfMixin {

  AdditionalProperties<Object
?> additionalProperties;

  
  @override
  UndefinedWrapper<
            int
> anyOf0;
  
  @override
  UndefinedWrapper<
    List<
        
            String
>
> anyOf1;
  

  ArrayAnyOf.$all({
        required this.additionalProperties,
    
    required this.anyOf0,
    required this.anyOf1,
  });

  ArrayAnyOf({
        this.additionalProperties = const AdditionalProperties(),
    
    this.anyOf0 = const UndefinedWrapper.undefined(),
    
    this.anyOf1 = const UndefinedWrapper.undefined(),
    
  });

  static const $reflection = ArrayAnyOfReflection.instance;
  ArrayAnyOfReflection get $classReflection => $reflection;

  @override
  bool validate() {
      
      final anyOfs = [anyOf0,anyOf1,].where((e) => e.isDefined).take(1);
      if (anyOfs.length == 0) {
        // there must be AT LEAST one "anyOf" schema.
        return false;
      }
      
    return super.validate();
  }

  Map<String, dynamic> toMap() {
    return _$ArrayAnyOfToMap(this);
  }
  factory ArrayAnyOf.fromMap(Map<String, dynamic> src) {
    return _$ArrayAnyOfFromMap(src);
  }
  static ArrayAnyOf? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return ArrayAnyOf.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$ArrayAnyOfCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory ArrayAnyOf.deserialize(Object? src) {
    return _$ArrayAnyOfDeserialize(src);
  }
  static ArrayAnyOf? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return ArrayAnyOf.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$ArrayAnyOfCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Object? serialize() {
    return _$ArrayAnyOfSerialize(this);
  }
}




