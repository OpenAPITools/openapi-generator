// Model reflection

part of 'apple.dart';


//class reflection

class AppleReflection extends ClassReflection<Apple> {
  static AppleReflection instanceGetter() => instance;
  static const instance = AppleReflection._(
    modelName: r'apple',
    className: r'Apple',
    cultivarPart: PropertyReflection<Apple, UndefinedWrapper<
            String
>>(
      dartName: r'cultivar',
      nullable: false,
      required: false,
      oasName: r'cultivar',
      oasType: r'string',
      pattern: r'/^[a-zA-Z\\s]*$/',
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _cultivarGetter,
      setter: _cultivarSetter,
    ),
    originPart: PropertyReflection<Apple, UndefinedWrapper<
            String
>>(
      dartName: r'origin',
      nullable: false,
      required: false,
      oasName: r'origin',
      oasType: r'string',
      pattern: r'/^[A-Z\\s]*$/i',
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _originGetter,
      setter: _originSetter,
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<Apple, Object
?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const AppleReflection._({
    required this.modelName,
    required this.className,
    required this.cultivarPart,
    required this.originPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<Apple, UndefinedWrapper<
            String
>> cultivarPart;
  static UndefinedWrapper<
            String
> _cultivarGetter(Apple parent) {
    return parent.cultivar;
  }
  static void _cultivarSetter(Apple parent, UndefinedWrapper<
            String
> value) {
    parent.cultivar = value;
  }
  final PropertyReflection<Apple, UndefinedWrapper<
            String
>> originPart;
  static UndefinedWrapper<
            String
> _originGetter(Apple parent) {
    return parent.origin;
  }
  static void _originSetter(Apple parent, UndefinedWrapper<
            String
> value) {
    parent.origin = value;
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
  List<PropertyReflection<Apple, dynamic>> get properties => [
    cultivarPart,
originPart,
  ];

  final AdditionalPropertiesReflection<Apple, Object
?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<Apple, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<Apple, dynamic>> get allOfs => [
    
  ];


  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => Apple.canDeserialize(src);
  @override
  Apple Function(Object? src) get deserializeFunction =>
      (src) => Apple.deserialize(src);

  @override
  Object? Function(Apple src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of Apple.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  Apple example({Map<String, (ClassReflection, PropertyReflection)> discriminators = const {}}) {
    final _reflection = this;
    if (discriminators.isEmpty) discriminators = _reflection.aggregatedDiscriminators;
    return Apple(
      cultivar: () {
        PartReflection? _partReflection = _reflection.cultivarPart;
        
        final disc = discriminators[r'cultivar'];
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
      origin: () {
        PartReflection? _partReflection = _reflection.originPart;
        
        final disc = discriminators[r'origin'];
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
      additionalProperties: () { PartReflection? _partReflection = _reflection.additionalPropertiesPart; return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
      
    );
  }
}

class AppleXmlReflection {
    const AppleXmlReflection();
}

