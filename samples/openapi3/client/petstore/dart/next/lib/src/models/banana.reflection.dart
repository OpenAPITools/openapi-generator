// Model reflection

part of 'banana.dart';


//class reflection

class BananaReflection extends ClassReflection<Banana> {
  static BananaReflection instanceGetter() => instance;
  static const instance = BananaReflection._(
    modelName: r'banana',
    className: r'Banana',
    lengthCmPart: PropertyReflection<Banana, UndefinedWrapper<
            num
>>(
      dartName: r'lengthCm',
      nullable: false,
      required: false,
      oasName: r'lengthCm',
      oasType: r'number',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _lengthCmGetter,
      setter: _lengthCmSetter,
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<Banana, Object
?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const BananaReflection._({
    required this.modelName,
    required this.className,
    required this.lengthCmPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<Banana, UndefinedWrapper<
            num
>> lengthCmPart;
  static UndefinedWrapper<
            num
> _lengthCmGetter(Banana parent) {
    return parent.lengthCm;
  }
  static void _lengthCmSetter(Banana parent, UndefinedWrapper<
            num
> value) {
    parent.lengthCm = value;
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
  List<PropertyReflection<Banana, dynamic>> get properties => [
    lengthCmPart,
  ];

  final AdditionalPropertiesReflection<Banana, Object
?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<Banana, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<Banana, dynamic>> get allOfs => [
    
  ];


  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => Banana.canDeserialize(src);
  @override
  Banana Function(Object? src) get deserializeFunction =>
      (src) => Banana.deserialize(src);

  @override
  Object? Function(Banana src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of Banana.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  Banana example({Map<String, (ClassReflection, PropertyReflection)> discriminators = const {}}) {
    final _reflection = this;
    if (discriminators.isEmpty) discriminators = _reflection.aggregatedDiscriminators;
    return Banana(
      lengthCm: () {
        PartReflection? _partReflection = _reflection.lengthCmPart;
        
        return UndefinedWrapper(


            
            


    
    examplenum()


);
      }(),
      additionalProperties: () { PartReflection? _partReflection = _reflection.additionalPropertiesPart; return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
      
    );
  }
}

class BananaXmlReflection {
    const BananaXmlReflection();
}

