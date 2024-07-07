// Model reflection

part of 'gm_fruit.dart';


//class reflection

class GmFruitReflection extends ClassReflection<GmFruit> {
  static GmFruitReflection instanceGetter() => instance;
  static const instance = GmFruitReflection._(
    modelName: r'gmFruit',
    className: r'GmFruit',
    colorPart: PropertyReflection<GmFruit, UndefinedWrapper<
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
    
    
    anyOf0Part: AnyOfReflection(
      parentReflectionGetter: instanceGetter,
      classReflection: AppleReflection.instance,
    ),
    
    anyOf1Part: AnyOfReflection(
      parentReflectionGetter: instanceGetter,
      classReflection: BananaReflection.instance,
    ),
    
  );
  const GmFruitReflection._({
    required this.modelName,
    required this.className,
    required this.colorPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.anyOf0Part,
    
    required this.anyOf1Part,
    
  });

  final PropertyReflection<GmFruit, UndefinedWrapper<
            String
>> colorPart;
  static UndefinedWrapper<
            String
> _colorGetter(GmFruit parent) {
    return parent.color;
  }
  static void _colorSetter(GmFruit parent, UndefinedWrapper<
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
  List<PropertyReflection<GmFruit, dynamic>> get properties => [
    colorPart,
  ];

  
  
  
  final AnyOfReflection<GmFruit, 
            Apple
?> anyOf0Part;
  
  final AnyOfReflection<GmFruit, 
            Banana
> anyOf1Part;
  
  @override
  List<PartReflection<GmFruit, dynamic>> get parts => [
    ...super.parts,
      ];
  @override
  List<AllOfReflection<GmFruit, dynamic>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<GmFruit, dynamic>> get oneOfs => [
    
  ];
  @override
  List<AnyOfReflection<GmFruit, dynamic>> get anyOfs => [
    anyOf0Part,anyOf1Part,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => GmFruit.canDeserialize(src);
  @override
  GmFruit Function(Object? src) get deserializeFunction =>
      (src) => GmFruit.deserialize(src);

  @override
  Object? Function(GmFruit src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of GmFruit.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  GmFruit example({Map<String, (ClassReflection, PropertyReflection)> discriminators = const {}}) {
    final _reflection = this;
    if (discriminators.isEmpty) discriminators = _reflection.aggregatedDiscriminators;
    return GmFruit(
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
      
      
      anyOf0: () {
        PartReflection? _partReflection = _reflection.anyOf0Part;
        return UndefinedWrapper(exampleNullable(() =>


            
            


    Apple.$reflection.example(discriminators: discriminators)
    


 ) );
      }(),
      anyOf1: () {
        PartReflection? _partReflection = _reflection.anyOf1Part;
        return UndefinedWrapper(


            
            


    Banana.$reflection.example(discriminators: discriminators)
    


);
      }(),
    );
  }
}

class GmFruitXmlReflection {
    const GmFruitXmlReflection();
}

