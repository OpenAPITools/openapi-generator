// Model reflection

part of 'map_test.dart';


//class reflection

class MapTestReflection extends ModelReflection<MapTest> {
  static MapTestReflection instanceGetter() => instance;
  static const instance = MapTestReflection._(
    modelName: r'MapTest',
    className: r'MapTest',
    xml: XmlReflection(
),
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
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_mapMapOfStringGetter),
      setter: FunctionWrapper2(_mapMapOfStringSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
    MapReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
    MapReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
)
,
)
)
,
)
),
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
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_mapOfEnumStringGetter),
      setter: FunctionWrapper2(_mapOfEnumStringSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
    MapReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            MapTestMapOfEnumStringEnum.$reflection
        
        
,
)
)
,
)
),
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
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_directMapGetter),
      setter: FunctionWrapper2(_directMapSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
    MapReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forbool
        
,
)
)
,
)
),
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
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_indirectMapGetter),
      setter: FunctionWrapper2(_indirectMapSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
    MapReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forbool
        
,
)
)
,
)
),
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesPart(
      parentReflectionGetter: instanceGetter,
      itemReflection: XmlReflectionWrapper(
    xml: XmlReflection(
),
    NullableReflection(ObjectReflection()
),
)
,
      getter: FunctionWrapper1(_AdditionalPropertiesGetter),
      setter: FunctionWrapper2(_AdditionalPropertiesSetter),
    ),
  );
  const MapTestReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
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
  final Map<String, ModelReflection> discriminatorMappings;
  @override
  final Map<String, ModelReflection> discriminatorImplicitMappings;
  @override
  final String? discriminatorKey;
  @override
  final String modelName;
  @override
  final String className;
  @override
  final XmlReflection xml;

  @override
  List<PropertyReflection<MapTest, dynamic>> get properties => [
    mapMapOfStringPart,
mapOfEnumStringPart,
directMapPart,
indirectMapPart,
  ];

  @override
  final AdditionalPropertiesPart<MapTest, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(MapTest instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(MapTest instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<MapTest, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  MapTest empty() {
    return MapTest(
    );
  }

  @override
  bool operator ==(Object other) {
    return other is MapTestReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


