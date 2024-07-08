// Model def

import 'package:petstore_api/_internal.dart';


part 'additional_properties_class.reflection.dart';
part 'additional_properties_class.serialization.dart';


/// AdditionalPropertiesClassMixin
///
/// Properties:
/// * [mapProperty] 
/// * [mapOfMapProperty] 
/// * [anytype1] 
/// * [mapWithUndeclaredPropertiesAnytype1] 
/// * [mapWithUndeclaredPropertiesAnytype2] 
/// * [mapWithUndeclaredPropertiesAnytype3] 
/// * [emptyMap] - an object with no declared properties and no undeclared properties, hence it's an empty map.
/// * [mapWithUndeclaredPropertiesString] 
mixin AdditionalPropertiesClassMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
    Map<String, 
        
            String

>

> get mapProperty;
UndefinedWrapper<
    Map<String, 
        
    Map<String, 
        
            String

>

>

> get mapOfMapProperty;
UndefinedWrapper<Object

?> get anytype1;
UndefinedWrapper<
            $FreeFormObject

> get mapWithUndeclaredPropertiesAnytype1;
UndefinedWrapper<
            $FreeFormObject

> get mapWithUndeclaredPropertiesAnytype2;
UndefinedWrapper<
    Map<String, 
        Object

?>

> get mapWithUndeclaredPropertiesAnytype3;
UndefinedWrapper<
            $FreeFormObject

> get emptyMap;
UndefinedWrapper<
    Map<String, 
        
            String

>

> get mapWithUndeclaredPropertiesString;
  
}

/// AdditionalPropertiesClass
///
/// Properties:
/// * [mapProperty] 
/// * [mapOfMapProperty] 
/// * [anytype1] 
/// * [mapWithUndeclaredPropertiesAnytype1] 
/// * [mapWithUndeclaredPropertiesAnytype2] 
/// * [mapWithUndeclaredPropertiesAnytype3] 
/// * [emptyMap] - an object with no declared properties and no undeclared properties, hence it's an empty map.
/// * [mapWithUndeclaredPropertiesString] 
class AdditionalPropertiesClass with
$OpenApiObjectMixin,

AdditionalPropertiesClassMixin {
  @override
  UndefinedWrapper<
    Map<String, 
        
            String

>

> mapProperty;
  @override
  UndefinedWrapper<
    Map<String, 
        
    Map<String, 
        
            String

>

>

> mapOfMapProperty;
  @override
  UndefinedWrapper<Object

?> anytype1;
  @override
  UndefinedWrapper<
            $FreeFormObject

> mapWithUndeclaredPropertiesAnytype1;
  @override
  UndefinedWrapper<
            $FreeFormObject

> mapWithUndeclaredPropertiesAnytype2;
  @override
  UndefinedWrapper<
    Map<String, 
        Object

?>

> mapWithUndeclaredPropertiesAnytype3;
  @override
  UndefinedWrapper<
            $FreeFormObject

> emptyMap;
  @override
  UndefinedWrapper<
    Map<String, 
        
            String

>

> mapWithUndeclaredPropertiesString;

  AdditionalProperties<Object

?> additionalProperties;

  

  AdditionalPropertiesClass.$all({
        required this.mapProperty,
    required this.mapOfMapProperty,
    required this.anytype1,
    required this.mapWithUndeclaredPropertiesAnytype1,
    required this.mapWithUndeclaredPropertiesAnytype2,
    required this.mapWithUndeclaredPropertiesAnytype3,
    required this.emptyMap,
    required this.mapWithUndeclaredPropertiesString,
    required this.additionalProperties,
    
  });

  AdditionalPropertiesClass({
      this.mapProperty = const UndefinedWrapper
        .undefined()
,
  this.mapOfMapProperty = const UndefinedWrapper
        .undefined()
,
  this.anytype1 = const UndefinedWrapper
        .undefined()
,
  this.mapWithUndeclaredPropertiesAnytype1 = const UndefinedWrapper
        .undefined()
,
  this.mapWithUndeclaredPropertiesAnytype2 = const UndefinedWrapper
        .undefined()
,
  this.mapWithUndeclaredPropertiesAnytype3 = const UndefinedWrapper
        .undefined()
,
  this.emptyMap = const UndefinedWrapper
        .undefined()
,
  this.mapWithUndeclaredPropertiesString = const UndefinedWrapper
        .undefined()
,
    this.additionalProperties = const AdditionalProperties(),
    
  });

  static const $reflection = AdditionalPropertiesClassReflection.instance;
  AdditionalPropertiesClassReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  Map<String, dynamic> toMap() {
    return _$AdditionalPropertiesClassToMap(this);
  }
  factory AdditionalPropertiesClass.fromMap(Map<String, dynamic> src) {
    return _$AdditionalPropertiesClassFromMap(src);
  }
  static AdditionalPropertiesClass? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return AdditionalPropertiesClass.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$AdditionalPropertiesClassCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory AdditionalPropertiesClass.deserialize(Object? src) {
    return _$AdditionalPropertiesClassDeserialize(src);
  }
  static AdditionalPropertiesClass? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return AdditionalPropertiesClass.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$AdditionalPropertiesClassCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Map<String,dynamic> serialize() {
    return _$AdditionalPropertiesClassSerialize(this);
  }
}




