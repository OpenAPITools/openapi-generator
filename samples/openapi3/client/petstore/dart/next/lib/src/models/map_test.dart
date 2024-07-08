// Model def

import 'package:petstore_api/_internal.dart';


part 'map_test.reflection.dart';
part 'map_test.serialization.dart';


/// MapTestMixin
///
/// Properties:
/// * [mapMapOfString] 
/// * [mapOfEnumString] 
/// * [directMap] 
/// * [indirectMap] 
mixin MapTestMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
    Map<String, 
        
    Map<String, 
        
            String

>

>

> get mapMapOfString;
UndefinedWrapper<
    Map<String, 
        
            MapTestMapOfEnumStringEnum

>

> get mapOfEnumString;
UndefinedWrapper<
    Map<String, 
        
            bool

>

> get directMap;
UndefinedWrapper<
    Map<String, 
        
            bool

>

> get indirectMap;
  
}

/// MapTest
///
/// Properties:
/// * [mapMapOfString] 
/// * [mapOfEnumString] 
/// * [directMap] 
/// * [indirectMap] 
class MapTest with
$OpenApiObjectMixin,

MapTestMixin {
  @override
  UndefinedWrapper<
    Map<String, 
        
    Map<String, 
        
            String

>

>

> mapMapOfString;
  @override
  UndefinedWrapper<
    Map<String, 
        
            MapTestMapOfEnumStringEnum

>

> mapOfEnumString;
  @override
  UndefinedWrapper<
    Map<String, 
        
            bool

>

> directMap;
  @override
  UndefinedWrapper<
    Map<String, 
        
            bool

>

> indirectMap;

  AdditionalProperties<Object

?> additionalProperties;

  

  MapTest.$all({
        required this.mapMapOfString,
    required this.mapOfEnumString,
    required this.directMap,
    required this.indirectMap,
    required this.additionalProperties,
    
  });

  MapTest({
      this.mapMapOfString = const UndefinedWrapper
        .undefined()
,
  this.mapOfEnumString = const UndefinedWrapper
        .undefined()
,
  this.directMap = const UndefinedWrapper
        .undefined()
,
  this.indirectMap = const UndefinedWrapper
        .undefined()
,
    this.additionalProperties = const AdditionalProperties(),
    
  });

  static const $reflection = MapTestReflection.instance;
  MapTestReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  Map<String, dynamic> toMap() {
    return _$MapTestToMap(this);
  }
  factory MapTest.fromMap(Map<String, dynamic> src) {
    return _$MapTestFromMap(src);
  }
  static MapTest? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return MapTest.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$MapTestCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory MapTest.deserialize(Object? src) {
    return _$MapTestDeserialize(src);
  }
  static MapTest? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return MapTest.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$MapTestCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Map<String,dynamic> serialize() {
    return _$MapTestSerialize(this);
  }
}




extension type const MapTestMapOfEnumStringEnum._(String value) {
      const MapTestMapOfEnumStringEnum.UPPER() : this._(r'UPPER');
      const MapTestMapOfEnumStringEnum.lower() : this._(r'lower');

  /// Creates a [MapTestMapOfEnumStringEnum] enum from a value and safely checking if it exists.
  factory MapTestMapOfEnumStringEnum.$safe(String value) {
    final res = values.where((element) => element.value == value).firstOrNull;
    if (res == null) {
      throw 'Invalid enum value $value';
    }
    return res;
  }

  static bool canDeserialize(Object? value) {
    return value is String && values.where((element) => element.value == value).firstOrNull != null;
  }

  /// Creates a [MapTestMapOfEnumStringEnum] enum from a value without checking if it exists.
  const MapTestMapOfEnumStringEnum.$unsafe(String value) : this._(value);

  /// All possible values of the enum.
  static const List<MapTestMapOfEnumStringEnum> values = [
    MapTestMapOfEnumStringEnum.UPPER(),
    MapTestMapOfEnumStringEnum.lower(),
    
  ];
}

