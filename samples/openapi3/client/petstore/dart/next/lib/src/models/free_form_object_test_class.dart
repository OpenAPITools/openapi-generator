// Model def

import 'package:openapi/_internal.dart';


part 'free_form_object_test_class.reflection.dart';
part 'free_form_object_test_class.serialization.dart';


/// FreeFormObjectTestClassMixin
///
/// Properties:
/// * [name] 
/// * [properties] 
mixin FreeFormObjectTestClassMixin on 
  $OpenApiObjectMixin {
  UndefinedWrapper<
            String
> get name;
UndefinedWrapper<
            FreeFormObjectTestClassProperties
> get properties;
  
}

/// FreeFormObjectTestClass
///
/// Properties:
/// * [name] 
/// * [properties] 
class FreeFormObjectTestClass with
$OpenApiObjectMixin,


FreeFormObjectTestClassMixin {
  @override
  UndefinedWrapper<
            String
> name;
  @override
  UndefinedWrapper<
            FreeFormObjectTestClassProperties
> properties;

  

  

  FreeFormObjectTestClass.$all({
        required this.name,
    required this.properties,
    
    
  });

  FreeFormObjectTestClass({
      this.name = const UndefinedWrapper
        .undefined()
,
  this.properties = const UndefinedWrapper
        .undefined()
,
    
    
  });

  static const $reflection = FreeFormObjectTestClassReflection.instance;

  @override
  bool validate() {
    return super.validate();
  }


  Map<String, dynamic> toMap() {
    return _$FreeFormObjectTestClassToMap(this);
  }
  factory FreeFormObjectTestClass.fromMap(Map<String, dynamic> src) {
    return _$FreeFormObjectTestClassFromMap(src);
  }
  static FreeFormObjectTestClass? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return FreeFormObjectTestClass.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$FreeFormObjectTestClassCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory FreeFormObjectTestClass.deserialize(Object? src) {
    return _$FreeFormObjectTestClassDeserialize(src);
  }
  static FreeFormObjectTestClass? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return FreeFormObjectTestClass.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$FreeFormObjectTestClassCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Object? serialize() {
    return _$FreeFormObjectTestClassSerialize(this);
  }
}




