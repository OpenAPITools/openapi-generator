// Model reflection

part of 'whale.dart';


//class reflection

class WhaleReflection extends ClassReflection<Whale> {
  static WhaleReflection instanceGetter() => instance;
  static const instance = WhaleReflection._(
    modelName: r'whale',
    className: r'Whale',
    hasBaleenPart: PropertyReflection<Whale, UndefinedWrapper<
            bool
>>(
      dartName: r'hasBaleen',
      nullable: false,
      required: false,
      oasName: r'hasBaleen',
      oasType: r'boolean',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _hasBaleenGetter,
      setter: _hasBaleenSetter,
    ),
    hasTeethPart: PropertyReflection<Whale, UndefinedWrapper<
            bool
>>(
      dartName: r'hasTeeth',
      nullable: false,
      required: false,
      oasName: r'hasTeeth',
      oasType: r'boolean',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _hasTeethGetter,
      setter: _hasTeethSetter,
    ),
    classNamePart: PropertyReflection<Whale, 
            String
>(
      dartName: r'className',
      nullable: false,
      required: true,
      oasName: r'className',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _classNameGetter,
      setter: _classNameSetter,
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<Whale, Object
?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const WhaleReflection._({
    required this.modelName,
    required this.className,
    required this.hasBaleenPart,
    required this.hasTeethPart,
    required this.classNamePart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<Whale, UndefinedWrapper<
            bool
>> hasBaleenPart;
  static UndefinedWrapper<
            bool
> _hasBaleenGetter(Whale parent) {
    return parent.hasBaleen;
  }
  static void _hasBaleenSetter(Whale parent, UndefinedWrapper<
            bool
> value) {
    parent.hasBaleen = value;
  }
  final PropertyReflection<Whale, UndefinedWrapper<
            bool
>> hasTeethPart;
  static UndefinedWrapper<
            bool
> _hasTeethGetter(Whale parent) {
    return parent.hasTeeth;
  }
  static void _hasTeethSetter(Whale parent, UndefinedWrapper<
            bool
> value) {
    parent.hasTeeth = value;
  }
  final PropertyReflection<Whale, 
            String
> classNamePart;
  static 
            String
 _classNameGetter(Whale parent) {
    return parent.className;
  }
  static void _classNameSetter(Whale parent, 
            String
 value) {
    parent.className = value;
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
  List<PropertyReflection<Whale, dynamic>> get properties => [
    hasBaleenPart,
hasTeethPart,
classNamePart,
  ];

  final AdditionalPropertiesReflection<Whale, Object
?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<Whale, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<Whale, dynamic>> get allOfs => [
    
  ];


  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => Whale.canDeserialize(src);
  @override
  Whale Function(Object? src) get deserializeFunction =>
      (src) => Whale.deserialize(src);

  @override
  Object? Function(Whale src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of Whale.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  Whale example({Map<String, (ClassReflection, PropertyReflection)> discriminators = const {}}) {
    final _reflection = this;
    if (discriminators.isEmpty) discriminators = _reflection.aggregatedDiscriminators;
    return Whale(
      hasBaleen: () {
        PartReflection? _partReflection = _reflection.hasBaleenPart;
        
        return UndefinedWrapper(


            
            


    
    examplebool()


);
      }(),
      hasTeeth: () {
        PartReflection? _partReflection = _reflection.hasTeethPart;
        
        return UndefinedWrapper(


            
            


    
    examplebool()


);
      }(),
      className: () {
        PartReflection? _partReflection = _reflection.classNamePart;
        
        final disc = discriminators[r'className'];
        if (disc != null) {
          final result = exampleDiscriminator(_partReflection, disc);
          if (result != null) {
            return result;
          }
        }
        
        return 


            
            


    
    exampleString()


;
      }(),
      additionalProperties: () { PartReflection? _partReflection = _reflection.additionalPropertiesPart; return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
      
    );
  }
}

class WhaleXmlReflection {
    const WhaleXmlReflection();
}

