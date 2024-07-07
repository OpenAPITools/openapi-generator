// Model reflection

part of 'additional_properties_class.dart';


//class reflection

class AdditionalPropertiesClassReflection extends ClassReflection<AdditionalPropertiesClass> {
  static AdditionalPropertiesClassReflection instanceGetter() => instance;
  static const instance = AdditionalPropertiesClassReflection._(
    modelName: r'AdditionalPropertiesClass',
    className: r'AdditionalPropertiesClass',
    mapPropertyPart: PropertyReflection<AdditionalPropertiesClass, UndefinedWrapper<
    Map<String, 
        
            String
>
>>(
      dartName: r'mapProperty',
      nullable: false,
      required: false,
      oasName: r'map_property',
      oasType: r'object',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      itemsReflection: ItemsReflection<AdditionalPropertiesClass, 
            String
>(parentReflectionGetter: instanceGetter,),
      getter: _mapPropertyGetter,
      setter: _mapPropertySetter,
    ),
    mapOfMapPropertyPart: PropertyReflection<AdditionalPropertiesClass, UndefinedWrapper<
    Map<String, 
        
    Map<String, 
        
            String
>
>
>>(
      dartName: r'mapOfMapProperty',
      nullable: false,
      required: false,
      oasName: r'map_of_map_property',
      oasType: r'object',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      itemsReflection: ItemsReflection<AdditionalPropertiesClass, 
    Map<String, 
        
            String
>
>(parentReflectionGetter: instanceGetter,itemsReflection: ItemsReflection<AdditionalPropertiesClass, 
            String
>(parentReflectionGetter: instanceGetter,)),
      getter: _mapOfMapPropertyGetter,
      setter: _mapOfMapPropertySetter,
    ),
    anytype1Part: PropertyReflection<AdditionalPropertiesClass, UndefinedWrapper<Object
?>>(
      dartName: r'anytype1',
      nullable: true,
      required: false,
      oasName: r'anytype_1',
      oasType: r'Object',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _anytype1Getter,
      setter: _anytype1Setter,
    ),
    mapWithUndeclaredPropertiesAnytype1Part: PropertyReflection<AdditionalPropertiesClass, UndefinedWrapper<
            $FreeFormObject
>>(
      dartName: r'mapWithUndeclaredPropertiesAnytype1',
      nullable: false,
      required: false,
      oasName: r'map_with_undeclared_properties_anytype_1',
      oasType: r'object',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _mapWithUndeclaredPropertiesAnytype1Getter,
      setter: _mapWithUndeclaredPropertiesAnytype1Setter,
    ),
    mapWithUndeclaredPropertiesAnytype2Part: PropertyReflection<AdditionalPropertiesClass, UndefinedWrapper<
            $FreeFormObject
>>(
      dartName: r'mapWithUndeclaredPropertiesAnytype2',
      nullable: false,
      required: false,
      oasName: r'map_with_undeclared_properties_anytype_2',
      oasType: r'object',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _mapWithUndeclaredPropertiesAnytype2Getter,
      setter: _mapWithUndeclaredPropertiesAnytype2Setter,
    ),
    mapWithUndeclaredPropertiesAnytype3Part: PropertyReflection<AdditionalPropertiesClass, UndefinedWrapper<
    Map<String, 
        Object
?>
>>(
      dartName: r'mapWithUndeclaredPropertiesAnytype3',
      nullable: false,
      required: false,
      oasName: r'map_with_undeclared_properties_anytype_3',
      oasType: r'object',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      itemsReflection: ItemsReflection<AdditionalPropertiesClass, Object
?>(parentReflectionGetter: instanceGetter,),
      getter: _mapWithUndeclaredPropertiesAnytype3Getter,
      setter: _mapWithUndeclaredPropertiesAnytype3Setter,
    ),
    emptyMapPart: PropertyReflection<AdditionalPropertiesClass, UndefinedWrapper<
            $FreeFormObject
>>(
      dartName: r'emptyMap',
      nullable: false,
      required: false,
      oasName: r'empty_map',
      oasType: r'object',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _emptyMapGetter,
      setter: _emptyMapSetter,
    ),
    mapWithUndeclaredPropertiesStringPart: PropertyReflection<AdditionalPropertiesClass, UndefinedWrapper<
    Map<String, 
        
            String
>
>>(
      dartName: r'mapWithUndeclaredPropertiesString',
      nullable: false,
      required: false,
      oasName: r'map_with_undeclared_properties_string',
      oasType: r'object',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      itemsReflection: ItemsReflection<AdditionalPropertiesClass, 
            String
>(parentReflectionGetter: instanceGetter,),
      getter: _mapWithUndeclaredPropertiesStringGetter,
      setter: _mapWithUndeclaredPropertiesStringSetter,
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<AdditionalPropertiesClass, Object
?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const AdditionalPropertiesClassReflection._({
    required this.modelName,
    required this.className,
    required this.mapPropertyPart,
    required this.mapOfMapPropertyPart,
    required this.anytype1Part,
    required this.mapWithUndeclaredPropertiesAnytype1Part,
    required this.mapWithUndeclaredPropertiesAnytype2Part,
    required this.mapWithUndeclaredPropertiesAnytype3Part,
    required this.emptyMapPart,
    required this.mapWithUndeclaredPropertiesStringPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<AdditionalPropertiesClass, UndefinedWrapper<
    Map<String, 
        
            String
>
>> mapPropertyPart;
  static UndefinedWrapper<
    Map<String, 
        
            String
>
> _mapPropertyGetter(AdditionalPropertiesClass parent) {
    return parent.mapProperty;
  }
  static void _mapPropertySetter(AdditionalPropertiesClass parent, UndefinedWrapper<
    Map<String, 
        
            String
>
> value) {
    parent.mapProperty = value;
  }
  final PropertyReflection<AdditionalPropertiesClass, UndefinedWrapper<
    Map<String, 
        
    Map<String, 
        
            String
>
>
>> mapOfMapPropertyPart;
  static UndefinedWrapper<
    Map<String, 
        
    Map<String, 
        
            String
>
>
> _mapOfMapPropertyGetter(AdditionalPropertiesClass parent) {
    return parent.mapOfMapProperty;
  }
  static void _mapOfMapPropertySetter(AdditionalPropertiesClass parent, UndefinedWrapper<
    Map<String, 
        
    Map<String, 
        
            String
>
>
> value) {
    parent.mapOfMapProperty = value;
  }
  final PropertyReflection<AdditionalPropertiesClass, UndefinedWrapper<Object
?>> anytype1Part;
  static UndefinedWrapper<Object
?> _anytype1Getter(AdditionalPropertiesClass parent) {
    return parent.anytype1;
  }
  static void _anytype1Setter(AdditionalPropertiesClass parent, UndefinedWrapper<Object
?> value) {
    parent.anytype1 = value;
  }
  final PropertyReflection<AdditionalPropertiesClass, UndefinedWrapper<
            $FreeFormObject
>> mapWithUndeclaredPropertiesAnytype1Part;
  static UndefinedWrapper<
            $FreeFormObject
> _mapWithUndeclaredPropertiesAnytype1Getter(AdditionalPropertiesClass parent) {
    return parent.mapWithUndeclaredPropertiesAnytype1;
  }
  static void _mapWithUndeclaredPropertiesAnytype1Setter(AdditionalPropertiesClass parent, UndefinedWrapper<
            $FreeFormObject
> value) {
    parent.mapWithUndeclaredPropertiesAnytype1 = value;
  }
  final PropertyReflection<AdditionalPropertiesClass, UndefinedWrapper<
            $FreeFormObject
>> mapWithUndeclaredPropertiesAnytype2Part;
  static UndefinedWrapper<
            $FreeFormObject
> _mapWithUndeclaredPropertiesAnytype2Getter(AdditionalPropertiesClass parent) {
    return parent.mapWithUndeclaredPropertiesAnytype2;
  }
  static void _mapWithUndeclaredPropertiesAnytype2Setter(AdditionalPropertiesClass parent, UndefinedWrapper<
            $FreeFormObject
> value) {
    parent.mapWithUndeclaredPropertiesAnytype2 = value;
  }
  final PropertyReflection<AdditionalPropertiesClass, UndefinedWrapper<
    Map<String, 
        Object
?>
>> mapWithUndeclaredPropertiesAnytype3Part;
  static UndefinedWrapper<
    Map<String, 
        Object
?>
> _mapWithUndeclaredPropertiesAnytype3Getter(AdditionalPropertiesClass parent) {
    return parent.mapWithUndeclaredPropertiesAnytype3;
  }
  static void _mapWithUndeclaredPropertiesAnytype3Setter(AdditionalPropertiesClass parent, UndefinedWrapper<
    Map<String, 
        Object
?>
> value) {
    parent.mapWithUndeclaredPropertiesAnytype3 = value;
  }
  final PropertyReflection<AdditionalPropertiesClass, UndefinedWrapper<
            $FreeFormObject
>> emptyMapPart;
  static UndefinedWrapper<
            $FreeFormObject
> _emptyMapGetter(AdditionalPropertiesClass parent) {
    return parent.emptyMap;
  }
  static void _emptyMapSetter(AdditionalPropertiesClass parent, UndefinedWrapper<
            $FreeFormObject
> value) {
    parent.emptyMap = value;
  }
  final PropertyReflection<AdditionalPropertiesClass, UndefinedWrapper<
    Map<String, 
        
            String
>
>> mapWithUndeclaredPropertiesStringPart;
  static UndefinedWrapper<
    Map<String, 
        
            String
>
> _mapWithUndeclaredPropertiesStringGetter(AdditionalPropertiesClass parent) {
    return parent.mapWithUndeclaredPropertiesString;
  }
  static void _mapWithUndeclaredPropertiesStringSetter(AdditionalPropertiesClass parent, UndefinedWrapper<
    Map<String, 
        
            String
>
> value) {
    parent.mapWithUndeclaredPropertiesString = value;
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
  List<PropertyReflection<AdditionalPropertiesClass, dynamic>> get properties => [
    mapPropertyPart,
mapOfMapPropertyPart,
anytype1Part,
mapWithUndeclaredPropertiesAnytype1Part,
mapWithUndeclaredPropertiesAnytype2Part,
mapWithUndeclaredPropertiesAnytype3Part,
emptyMapPart,
mapWithUndeclaredPropertiesStringPart,
  ];

  final AdditionalPropertiesReflection<AdditionalPropertiesClass, Object
?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<AdditionalPropertiesClass, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<AdditionalPropertiesClass, dynamic>> get allOfs => [
    
  ];


  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => AdditionalPropertiesClass.canDeserialize(src);
  @override
  AdditionalPropertiesClass Function(Object? src) get deserializeFunction =>
      (src) => AdditionalPropertiesClass.deserialize(src);

  @override
  Object? Function(AdditionalPropertiesClass src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of AdditionalPropertiesClass.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  AdditionalPropertiesClass example({Map<String, (ClassReflection, PropertyReflection)> discriminators = const {}}) {
    final _reflection = this;
    if (discriminators.isEmpty) discriminators = _reflection.aggregatedDiscriminators;
    return AdditionalPropertiesClass(
      mapProperty: () {
        PartReflection? _partReflection = _reflection.mapPropertyPart;
        
        return UndefinedWrapper(


    exampleMap(() { _partReflection = _partReflection?.itemsReflection; return 


            
            


    
    exampleString()


; })



);
      }(),
      mapOfMapProperty: () {
        PartReflection? _partReflection = _reflection.mapOfMapPropertyPart;
        
        return UndefinedWrapper(


    exampleMap(() { _partReflection = _partReflection?.itemsReflection; return 


    exampleMap(() { _partReflection = _partReflection?.itemsReflection; return 


            
            


    
    exampleString()


; })



; })



);
      }(),
      anytype1: () {
        PartReflection? _partReflection = _reflection.anytype1Part;
        
        return UndefinedWrapper(exampleNullable(() =>

exampleObject()



 ) );
      }(),
      mapWithUndeclaredPropertiesAnytype1: () {
        PartReflection? _partReflection = _reflection.mapWithUndeclaredPropertiesAnytype1Part;
        
        return UndefinedWrapper(


            
            


    
    example$FreeFormObject()


);
      }(),
      mapWithUndeclaredPropertiesAnytype2: () {
        PartReflection? _partReflection = _reflection.mapWithUndeclaredPropertiesAnytype2Part;
        
        return UndefinedWrapper(


            
            


    
    example$FreeFormObject()


);
      }(),
      mapWithUndeclaredPropertiesAnytype3: () {
        PartReflection? _partReflection = _reflection.mapWithUndeclaredPropertiesAnytype3Part;
        
        return UndefinedWrapper(


    exampleMap(() { _partReflection = _partReflection?.itemsReflection; return exampleNullable(() =>

exampleObject()



 ) ; })



);
      }(),
      emptyMap: () {
        PartReflection? _partReflection = _reflection.emptyMapPart;
        
        return UndefinedWrapper(


            
            


    
    example$FreeFormObject()


);
      }(),
      mapWithUndeclaredPropertiesString: () {
        PartReflection? _partReflection = _reflection.mapWithUndeclaredPropertiesStringPart;
        
        return UndefinedWrapper(


    exampleMap(() { _partReflection = _partReflection?.itemsReflection; return 


            
            


    
    exampleString()


; })



);
      }(),
      additionalProperties: () { PartReflection? _partReflection = _reflection.additionalPropertiesPart; return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
      
    );
  }
}

class AdditionalPropertiesClassXmlReflection {
    const AdditionalPropertiesClassXmlReflection();
}

