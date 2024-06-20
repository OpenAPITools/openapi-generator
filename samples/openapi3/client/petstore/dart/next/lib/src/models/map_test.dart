// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'map_test.reflection.dart';
part 'map_test.serialization.dart';


//class defination

///
mixin MapTestMixin on 
  
  $OpenApiObjectMixin
 {
  UndefinedWrapper<Map<String, Map<String, String>>> get mapMapOfString;
  UndefinedWrapper<Map<String, InnerEnum>> get mapOfEnumString;
  UndefinedWrapper<Map<String, bool>> get directMap;
  UndefinedWrapper<Map<String, bool>> get indirectMap;


}

///
class MapTest with
$OpenApiObjectMixin,


MapTestMixin {
  @override
  UndefinedWrapper<Map<String, Map<String, String>>> mapMapOfString;
  @override
  UndefinedWrapper<Map<String, InnerEnum>> mapOfEnumString;
  @override
  UndefinedWrapper<Map<String, bool>> directMap;
  @override
  UndefinedWrapper<Map<String, bool>> indirectMap;





  MapTest.$all({
    required this.mapMapOfString,
    required this.mapOfEnumString,
    required this.directMap,
    required this.indirectMap,
    
    
  });

  MapTest({
    this.mapMapOfString = const UndefinedWrapper.undefined(),
    this.mapOfEnumString = const UndefinedWrapper.undefined(),
    this.directMap = const UndefinedWrapper.undefined(),
    this.indirectMap = const UndefinedWrapper.undefined(),
    
    
  });
}




//inline enum def

extension type const MapOfEnumStringEnum._(String value) {
      const MapOfEnumStringEnum.UPPER() : this._(r'UPPER');
      const MapOfEnumStringEnum.lower() : this._(r'lower');

  /// Creates a [MapOfEnumStringEnum] enum from a value and safely checking if it exists.
  factory MapOfEnumStringEnum.$safe(String value) {
    final res = values.where((element) => element.value == value).firstOrNull;
    if (res == null) {
      throw 'Invalid enum value $value';
    }
    return res;
  }

  /// Creates a [MapOfEnumStringEnum] enum from a value without checking if it exists.
  const MapOfEnumStringEnum.$unsafe(String value) : this._(value);

  /// All possible values of the enum.
  static const List<MapOfEnumStringEnum> values = [
    MapOfEnumStringEnum.UPPER(),
    MapOfEnumStringEnum.lower(),
    
  ];
}

