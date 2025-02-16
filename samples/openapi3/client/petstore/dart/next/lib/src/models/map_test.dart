// Model def

import 'package:petstore_api/_internal.dart';


part 'map_test.reflection.dart';


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
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = MapTestReflection.instance;
  MapTestReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory MapTest.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  MapTest clone() {
    return $reflection.clone(this);
  }
}









extension type const MapTestMapOfEnumStringEnum._(String value) implements String {
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

  static const $reflection = EnumReflection<MapTestMapOfEnumStringEnum, String>(
    PrimitiveReflection.forString,
    members: [
      
        EnumMemberReflection(dartName: r'UPPER', oasValue: r'UPPER', value: MapTestMapOfEnumStringEnum.UPPER()),
      
        EnumMemberReflection(dartName: r'lower', oasValue: r'lower', value: MapTestMapOfEnumStringEnum.lower()),
      
    ],
  );

  factory MapTestMapOfEnumStringEnum.deserialize(Object? value, [SerializationContext context = const SerializationContext.json()]) {
    return $reflection.deserializeFunction(value, context);
  }

  static bool canDeserialize(Object? value, [SerializationContext context = const SerializationContext.json()]) {
    return $reflection.canDeserializeFunction(value,context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json()]) {
    return $reflection.serializeFunction(this, context);
  }

  /// Creates a [MapTestMapOfEnumStringEnum] enum from a value without checking if it exists.
  const MapTestMapOfEnumStringEnum.$unsafe(String value) : this._(value);

  /// All possible values of the enum.
  static const List<MapTestMapOfEnumStringEnum> values = [
    MapTestMapOfEnumStringEnum.UPPER(),
    MapTestMapOfEnumStringEnum.lower(),
    
  ];
}














