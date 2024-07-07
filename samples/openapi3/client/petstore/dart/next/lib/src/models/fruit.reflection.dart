// Model reflection

part of 'fruit.dart';


//class reflection

class FruitReflection extends ClassReflection<Fruit> {
  static FruitReflection instanceGetter() => instance;
  static const instance = FruitReflection._(
    modelName: r'fruit',
    className: r'Fruit',
    colorPart: PropertyReflection<Fruit, UndefinedWrapper<
            String
>>(
      dartName: r'color',
      nullable: false,
      required: false,
      oasName: r'color',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _colorGetter,
      setter: _colorSetter,
    ),
    
    
    oneOf0Part: OneOfReflection(
      parentReflectionGetter: instanceGetter,
      classReflection: AppleReflection.instance,
    ),
    
    oneOf1Part: OneOfReflection(
      parentReflectionGetter: instanceGetter,
      classReflection: BananaReflection.instance,
    ),
    
  );
  const FruitReflection._({
    required this.modelName,
    required this.className,
    required this.colorPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.oneOf0Part,
    
    required this.oneOf1Part,
    
  });

  final PropertyReflection<Fruit, UndefinedWrapper<
            String
>> colorPart;
  static UndefinedWrapper<
            String
> _colorGetter(Fruit parent) {
    return parent.color;
  }
  static void _colorSetter(Fruit parent, UndefinedWrapper<
            String
> value) {
    parent.color = value;
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
  List<PropertyReflection<Fruit, dynamic>> get properties => [
    colorPart,
  ];

  
  
  
  final OneOfReflection<Fruit, 
            Apple
?> oneOf0Part;
  
  final OneOfReflection<Fruit, 
            Banana
> oneOf1Part;
  
  @override
  List<PartReflection<Fruit, dynamic>> get parts => [
    ...super.parts,
      ];
  @override
  List<AllOfReflection<Fruit, dynamic>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<Fruit, dynamic>> get oneOfs => [
    oneOf0Part,oneOf1Part,
  ];
  @override
  List<AnyOfReflection<Fruit, dynamic>> get anyOfs => [
    
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => Fruit.canDeserialize(src);
  @override
  Fruit Function(Object? src) get deserializeFunction =>
      (src) => Fruit.deserialize(src);

  @override
  Object? Function(Fruit src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of Fruit.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  Fruit example({Map<String, (ClassReflection, PropertyReflection)> discriminators = const {}}) {
    final _reflection = this;
    if (discriminators.isEmpty) discriminators = _reflection.aggregatedDiscriminators;
    return Fruit(
      color: () {
        PartReflection? _partReflection = _reflection.colorPart;
        
        final disc = discriminators[r'color'];
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
      
      
      oneOf0: () {
        PartReflection? _partReflection = _reflection.oneOf0Part;
        return UndefinedWrapper(exampleNullable(() =>


            
            


    Apple.$reflection.example(discriminators: discriminators)
    


 ) );
      }(),
      
    );
  }
}

class FruitXmlReflection {
    const FruitXmlReflection();
}

