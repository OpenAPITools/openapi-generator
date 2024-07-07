// Model reflection

part of 'mixed_properties_and_additional_properties_class.dart';


//class reflection

class MixedPropertiesAndAdditionalPropertiesClassReflection extends ClassReflection<MixedPropertiesAndAdditionalPropertiesClass> {
  static MixedPropertiesAndAdditionalPropertiesClassReflection instanceGetter() => instance;
  static const instance = MixedPropertiesAndAdditionalPropertiesClassReflection._(
    modelName: r'MixedPropertiesAndAdditionalPropertiesClass',
    className: r'MixedPropertiesAndAdditionalPropertiesClass',
    uuidPart: PropertyReflection<MixedPropertiesAndAdditionalPropertiesClass, UndefinedWrapper<
            String
>>(
      dartName: r'uuid',
      nullable: false,
      required: false,
      oasName: r'uuid',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _uuidGetter,
      setter: _uuidSetter,
    ),
    dateTimePart: PropertyReflection<MixedPropertiesAndAdditionalPropertiesClass, UndefinedWrapper<
            DateTime
>>(
      dartName: r'dateTime',
      nullable: false,
      required: false,
      oasName: r'dateTime',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _dateTimeGetter,
      setter: _dateTimeSetter,
    ),
    mapPart: PropertyReflection<MixedPropertiesAndAdditionalPropertiesClass, UndefinedWrapper<
    Map<String, 
        
            Animal
>
>>(
      dartName: r'map',
      nullable: false,
      required: false,
      oasName: r'map',
      oasType: r'object',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      itemsReflection: ItemsReflection<MixedPropertiesAndAdditionalPropertiesClass, 
            Animal
>(parentReflectionGetter: instanceGetter,classReflection: AnimalReflection.instance,),
      getter: _mapGetter,
      setter: _mapSetter,
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<MixedPropertiesAndAdditionalPropertiesClass, Object
?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const MixedPropertiesAndAdditionalPropertiesClassReflection._({
    required this.modelName,
    required this.className,
    required this.uuidPart,
    required this.dateTimePart,
    required this.mapPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<MixedPropertiesAndAdditionalPropertiesClass, UndefinedWrapper<
            String
>> uuidPart;
  static UndefinedWrapper<
            String
> _uuidGetter(MixedPropertiesAndAdditionalPropertiesClass parent) {
    return parent.uuid;
  }
  static void _uuidSetter(MixedPropertiesAndAdditionalPropertiesClass parent, UndefinedWrapper<
            String
> value) {
    parent.uuid = value;
  }
  final PropertyReflection<MixedPropertiesAndAdditionalPropertiesClass, UndefinedWrapper<
            DateTime
>> dateTimePart;
  static UndefinedWrapper<
            DateTime
> _dateTimeGetter(MixedPropertiesAndAdditionalPropertiesClass parent) {
    return parent.dateTime;
  }
  static void _dateTimeSetter(MixedPropertiesAndAdditionalPropertiesClass parent, UndefinedWrapper<
            DateTime
> value) {
    parent.dateTime = value;
  }
  final PropertyReflection<MixedPropertiesAndAdditionalPropertiesClass, UndefinedWrapper<
    Map<String, 
        
            Animal
>
>> mapPart;
  static UndefinedWrapper<
    Map<String, 
        
            Animal
>
> _mapGetter(MixedPropertiesAndAdditionalPropertiesClass parent) {
    return parent.map;
  }
  static void _mapSetter(MixedPropertiesAndAdditionalPropertiesClass parent, UndefinedWrapper<
    Map<String, 
        
            Animal
>
> value) {
    parent.map = value;
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
  List<PropertyReflection<MixedPropertiesAndAdditionalPropertiesClass, dynamic>> get properties => [
    uuidPart,
dateTimePart,
mapPart,
  ];

  final AdditionalPropertiesReflection<MixedPropertiesAndAdditionalPropertiesClass, Object
?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<MixedPropertiesAndAdditionalPropertiesClass, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<MixedPropertiesAndAdditionalPropertiesClass, dynamic>> get allOfs => [
    
  ];


  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => MixedPropertiesAndAdditionalPropertiesClass.canDeserialize(src);
  @override
  MixedPropertiesAndAdditionalPropertiesClass Function(Object? src) get deserializeFunction =>
      (src) => MixedPropertiesAndAdditionalPropertiesClass.deserialize(src);

  @override
  Object? Function(MixedPropertiesAndAdditionalPropertiesClass src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of MixedPropertiesAndAdditionalPropertiesClass.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  MixedPropertiesAndAdditionalPropertiesClass example({Map<String, (ClassReflection, PropertyReflection)> discriminators = const {}}) {
    final _reflection = this;
    if (discriminators.isEmpty) discriminators = _reflection.aggregatedDiscriminators;
    return MixedPropertiesAndAdditionalPropertiesClass(
      uuid: () {
        PartReflection? _partReflection = _reflection.uuidPart;
        
        final disc = discriminators[r'uuid'];
        if (disc != null) {
          final result = exampleDiscriminator(_partReflection, disc);
          if (result != null) {
            return UndefinedWrapper(result);
          }
        }
        
        return UndefinedWrapper(


            
            


    
    exampleString()


);
      }(),
      dateTime: () {
        PartReflection? _partReflection = _reflection.dateTimePart;
        
        return UndefinedWrapper(


            
            


    
    exampleDateTime()


);
      }(),
      map: () {
        PartReflection? _partReflection = _reflection.mapPart;
        
        return UndefinedWrapper(


    exampleMap(() { _partReflection = _partReflection?.itemsReflection; return 


            
            


    Animal.$reflection.example()
    


; })



);
      }(),
      additionalProperties: () { PartReflection? _partReflection = _reflection.additionalPropertiesPart; return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
      
    );
  }
}

class MixedPropertiesAndAdditionalPropertiesClassXmlReflection {
    const MixedPropertiesAndAdditionalPropertiesClassXmlReflection();
}

