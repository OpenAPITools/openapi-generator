// Model reflection

part of 'map_test.dart';


//class reflection

class MapTestReflection extends ClassReflection<MapTest> {
  static const instance = MapTestReflection._(
    mapMapOfString: PropertyReflection(
      dartName: r'mapMapOfString',
      nullable: false,
      required: false,
      oasName: r'map_map_of_string',
      oasType: r'object',
      pattern: null,
    ),
    mapOfEnumString: PropertyReflection(
      dartName: r'mapOfEnumString',
      nullable: false,
      required: false,
      oasName: r'map_of_enum_string',
      oasType: r'object',
      pattern: null,
    ),
    directMap: PropertyReflection(
      dartName: r'directMap',
      nullable: false,
      required: false,
      oasName: r'direct_map',
      oasType: r'object',
      pattern: null,
    ),
    indirectMap: PropertyReflection(
      dartName: r'indirectMap',
      nullable: false,
      required: false,
      oasName: r'indirect_map',
      oasType: r'object',
      pattern: null,
    ),
  );
  const MapTestReflection._({
    required this.mapMapOfString,
  
    required this.mapOfEnumString,
  
    required this.directMap,
  
    required this.indirectMap,
  });

  final PropertyReflection<UndefinedWrapper<
    Map<String, 
        
    Map<String, 
        
            String
>
>
>> mapMapOfString;
  final PropertyReflection<UndefinedWrapper<
    Map<String, 
        
            MapTestMapOfEnumStringEnum
>
>> mapOfEnumString;
  final PropertyReflection<UndefinedWrapper<
    Map<String, 
        
            bool
>
>> directMap;
  final PropertyReflection<UndefinedWrapper<
    Map<String, 
        
            bool
>
>> indirectMap;

  @override
  List<PropertyReflection> get members => [
    mapMapOfString,
mapOfEnumString,
directMap,
indirectMap,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => MapTest.canDeserialize(src);
  @override
  MapTest Function(Object? src) get deserializeFunction =>
      (src) => MapTest.deserialize(src);

  @override
  Object? Function(MapTest src) get serializeFunction =>
      (src) => src.serialize();
}

class MapTestXmlReflection {
    const MapTestXmlReflection();
}

