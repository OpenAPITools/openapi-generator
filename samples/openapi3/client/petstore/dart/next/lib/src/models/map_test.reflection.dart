// Model reflection

part of 'map_test.dart';


//class reflection

class MapTestReflection extends ClassReflection<MapTest> {
  static MapTestReflection instanceGetter() => instance;
  static const instance = MapTestReflection._(
    modelName: r'MapTest',
    className: r'MapTest',
    mapMapOfStringPart: PropertyReflection<MapTest, UndefinedWrapper<
    Map<String, 
        
    Map<String, 
        
            String
>
>
>>(
      dartName: r'mapMapOfString',
      nullable: false,
      required: false,
      oasName: r'map_map_of_string',
      oasType: r'object',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      itemsReflection: ItemsReflection<MapTest, 
    Map<String, 
        
            String
>
>(parentReflectionGetter: instanceGetter,itemsReflection: ItemsReflection<MapTest, 
            String
>(parentReflectionGetter: instanceGetter,)),
      getter: _mapMapOfStringGetter,
      setter: _mapMapOfStringSetter,
    ),
    mapOfEnumStringPart: PropertyReflection<MapTest, UndefinedWrapper<
    Map<String, 
        
            MapTestMapOfEnumStringEnum
>
>>(
      dartName: r'mapOfEnumString',
      nullable: false,
      required: false,
      oasName: r'map_of_enum_string',
      oasType: r'object',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      itemsReflection: ItemsReflection<MapTest, 
            MapTestMapOfEnumStringEnum
>(parentReflectionGetter: instanceGetter,),
      getter: _mapOfEnumStringGetter,
      setter: _mapOfEnumStringSetter,
    ),
    directMapPart: PropertyReflection<MapTest, UndefinedWrapper<
    Map<String, 
        
            bool
>
>>(
      dartName: r'directMap',
      nullable: false,
      required: false,
      oasName: r'direct_map',
      oasType: r'object',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      itemsReflection: ItemsReflection<MapTest, 
            bool
>(parentReflectionGetter: instanceGetter,),
      getter: _directMapGetter,
      setter: _directMapSetter,
    ),
    indirectMapPart: PropertyReflection<MapTest, UndefinedWrapper<
    Map<String, 
        
            bool
>
>>(
      dartName: r'indirectMap',
      nullable: false,
      required: false,
      oasName: r'indirect_map',
      oasType: r'object',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      itemsReflection: ItemsReflection<MapTest, 
            bool
>(parentReflectionGetter: instanceGetter,),
      getter: _indirectMapGetter,
      setter: _indirectMapSetter,
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<MapTest, Object
?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const MapTestReflection._({
    required this.modelName,
    required this.className,
    required this.mapMapOfStringPart,
    required this.mapOfEnumStringPart,
    required this.directMapPart,
    required this.indirectMapPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<MapTest, UndefinedWrapper<
    Map<String, 
        
    Map<String, 
        
            String
>
>
>> mapMapOfStringPart;
  static UndefinedWrapper<
    Map<String, 
        
    Map<String, 
        
            String
>
>
> _mapMapOfStringGetter(MapTest parent) {
    return parent.mapMapOfString;
  }
  static void _mapMapOfStringSetter(MapTest parent, UndefinedWrapper<
    Map<String, 
        
    Map<String, 
        
            String
>
>
> value) {
    parent.mapMapOfString = value;
  }
  final PropertyReflection<MapTest, UndefinedWrapper<
    Map<String, 
        
            MapTestMapOfEnumStringEnum
>
>> mapOfEnumStringPart;
  static UndefinedWrapper<
    Map<String, 
        
            MapTestMapOfEnumStringEnum
>
> _mapOfEnumStringGetter(MapTest parent) {
    return parent.mapOfEnumString;
  }
  static void _mapOfEnumStringSetter(MapTest parent, UndefinedWrapper<
    Map<String, 
        
            MapTestMapOfEnumStringEnum
>
> value) {
    parent.mapOfEnumString = value;
  }
  final PropertyReflection<MapTest, UndefinedWrapper<
    Map<String, 
        
            bool
>
>> directMapPart;
  static UndefinedWrapper<
    Map<String, 
        
            bool
>
> _directMapGetter(MapTest parent) {
    return parent.directMap;
  }
  static void _directMapSetter(MapTest parent, UndefinedWrapper<
    Map<String, 
        
            bool
>
> value) {
    parent.directMap = value;
  }
  final PropertyReflection<MapTest, UndefinedWrapper<
    Map<String, 
        
            bool
>
>> indirectMapPart;
  static UndefinedWrapper<
    Map<String, 
        
            bool
>
> _indirectMapGetter(MapTest parent) {
    return parent.indirectMap;
  }
  static void _indirectMapSetter(MapTest parent, UndefinedWrapper<
    Map<String, 
        
            bool
>
> value) {
    parent.indirectMap = value;
  }



  @override
  final Map<String, ClassReflection> discriminatorMappings;
  @override
  final Map<String, ClassReflection> discriminatorImplicitMappings;
  @override
  final String? discriminatorKey;
  @override
  final String modelName;
  @override
  final String className;


  @override
  List<PropertyReflection<MapTest, dynamic>> get properties => [
    mapMapOfStringPart,
mapOfEnumStringPart,
directMapPart,
indirectMapPart,
  ];

  final AdditionalPropertiesReflection<MapTest, Object
?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<MapTest, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<MapTest, dynamic>> get allOfs => [
    
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

  /// Gets an example of MapTest.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  MapTest example({Map<String, (ClassReflection, PropertyReflection)> discriminators = const {}}) {
    final _reflection = this;
    if (discriminators.isEmpty) discriminators = _reflection.aggregatedDiscriminators;
    return MapTest(
      mapMapOfString: () {
        PartReflection? _partReflection = _reflection.mapMapOfStringPart;
        
        return UndefinedWrapper(


    exampleMap(() { _partReflection = _partReflection?.itemsReflection; return 


    exampleMap(() { _partReflection = _partReflection?.itemsReflection; return 


            
            


    
    exampleString()


; })



; })



);
      }(),
      mapOfEnumString: () {
        PartReflection? _partReflection = _reflection.mapOfEnumStringPart;
        
        return UndefinedWrapper(


    exampleMap(() { _partReflection = _partReflection?.itemsReflection; return 


            exampleEnum(MapTestMapOfEnumStringEnum.values)



; })



);
      }(),
      directMap: () {
        PartReflection? _partReflection = _reflection.directMapPart;
        
        return UndefinedWrapper(


    exampleMap(() { _partReflection = _partReflection?.itemsReflection; return 


            
            


    
    examplebool()


; })



);
      }(),
      indirectMap: () {
        PartReflection? _partReflection = _reflection.indirectMapPart;
        
        return UndefinedWrapper(


    exampleMap(() { _partReflection = _partReflection?.itemsReflection; return 


            
            


    
    examplebool()


; })



);
      }(),
      additionalProperties: () { PartReflection? _partReflection = _reflection.additionalPropertiesPart; return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
      
    );
  }
}

class MapTestXmlReflection {
    const MapTestXmlReflection();
}

