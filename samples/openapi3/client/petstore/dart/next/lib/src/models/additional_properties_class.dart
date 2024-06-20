// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

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
  
  $OpenApiObjectMixin
 {
  UndefinedWrapper<Map<String, String>> get mapProperty;
  UndefinedWrapper<Map<String, Map<String, String>>> get mapOfMapProperty;
  UndefinedWrapper<Object?> get anytype1;
  UndefinedWrapper<$OpenApiObjectMixin> get mapWithUndeclaredPropertiesAnytype1;
  UndefinedWrapper<$OpenApiObjectMixin> get mapWithUndeclaredPropertiesAnytype2;
  UndefinedWrapper<Map<String, Object?>> get mapWithUndeclaredPropertiesAnytype3;
  UndefinedWrapper<$OpenApiObjectMixin> get emptyMap;
  UndefinedWrapper<Map<String, String>> get mapWithUndeclaredPropertiesString;

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
  UndefinedWrapper<Map<String, String>> mapProperty;
  @override
  UndefinedWrapper<Map<String, Map<String, String>>> mapOfMapProperty;
  @override
  UndefinedWrapper<Object?> anytype1;
  @override
  UndefinedWrapper<$OpenApiObjectMixin> mapWithUndeclaredPropertiesAnytype1;
  @override
  UndefinedWrapper<$OpenApiObjectMixin> mapWithUndeclaredPropertiesAnytype2;
  @override
  UndefinedWrapper<Map<String, Object?>> mapWithUndeclaredPropertiesAnytype3;
  @override
  UndefinedWrapper<$OpenApiObjectMixin> emptyMap;
  @override
  UndefinedWrapper<Map<String, String>> mapWithUndeclaredPropertiesString;





  AdditionalPropertiesClass.$all({
    required this.mapProperty,
    required this.mapOfMapProperty,
    required this.anytype1,
    required this.mapWithUndeclaredPropertiesAnytype1,
    required this.mapWithUndeclaredPropertiesAnytype2,
    required this.mapWithUndeclaredPropertiesAnytype3,
    required this.emptyMap,
    required this.mapWithUndeclaredPropertiesString,
    
    
  });

  AdditionalPropertiesClass({
    this.mapProperty = const UndefinedWrapper.undefined(),
    this.mapOfMapProperty = const UndefinedWrapper.undefined(),
    this.anytype1 = const UndefinedWrapper.undefined(),
    this.mapWithUndeclaredPropertiesAnytype1 = const UndefinedWrapper.undefined(),
    this.mapWithUndeclaredPropertiesAnytype2 = const UndefinedWrapper.undefined(),
    this.mapWithUndeclaredPropertiesAnytype3 = const UndefinedWrapper.undefined(),
    this.emptyMap = const UndefinedWrapper.undefined(),
    this.mapWithUndeclaredPropertiesString = const UndefinedWrapper.undefined(),
    
    
  });
}




