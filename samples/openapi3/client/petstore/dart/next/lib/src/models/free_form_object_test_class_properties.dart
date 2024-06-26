// Model def

import 'package:openapi/_internal.dart';


part 'free_form_object_test_class_properties.reflection.dart';
part 'free_form_object_test_class_properties.serialization.dart';


/// FreeFormObjectTestClassPropertiesMixin
mixin FreeFormObjectTestClassPropertiesMixin on
  $OpenApiObjectMixin {
    
  UndefinedWrapper<
            String
> get oneOf0;
  UndefinedWrapper<
    Map<String, 
        Object
?>
> get oneOf1;
}

/// FreeFormObjectTestClassProperties
class FreeFormObjectTestClassProperties with
$OpenApiObjectMixin,


FreeFormObjectTestClassPropertiesMixin {

  AdditionalProperties<Object
?> additionalProperties;

  
  @override
  UndefinedWrapper<
            String
> oneOf0;
  
  @override
  UndefinedWrapper<
    Map<String, 
        Object
?>
> oneOf1;
  

  FreeFormObjectTestClassProperties.$all({
        required this.additionalProperties,
    
    required this.oneOf0,
    required this.oneOf1,
  });

  FreeFormObjectTestClassProperties({
        this.additionalProperties = const AdditionalProperties(),
    
    this.oneOf0 = const UndefinedWrapper.undefined(),
    this.oneOf1 = const UndefinedWrapper.undefined(),
  });

  static const $reflection = FreeFormObjectTestClassPropertiesReflection.instance;

  @override
  bool validate() {
      final oneOfs = [oneOf0,oneOf1,].where((e) => e.isDefined).take(2);
      if (oneOfs.length != 1) {
        // there must be EXACTLY one "oneOf" schema.
        return false;
      }
      
      
    return super.validate();
  }


  Map<String, dynamic> toMap() {
    return _$FreeFormObjectTestClassPropertiesToMap(this);
  }
  factory FreeFormObjectTestClassProperties.fromMap(Map<String, dynamic> src) {
    return _$FreeFormObjectTestClassPropertiesFromMap(src);
  }
  static FreeFormObjectTestClassProperties? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return FreeFormObjectTestClassProperties.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$FreeFormObjectTestClassPropertiesCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory FreeFormObjectTestClassProperties.deserialize(Object? src) {
    return _$FreeFormObjectTestClassPropertiesDeserialize(src);
  }
  static FreeFormObjectTestClassProperties? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return FreeFormObjectTestClassProperties.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$FreeFormObjectTestClassPropertiesCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Object? serialize() {
    return _$FreeFormObjectTestClassPropertiesSerialize(this);
  }
}




