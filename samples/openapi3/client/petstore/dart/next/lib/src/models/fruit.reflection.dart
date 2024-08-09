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
    
    
    oneOf0Part: FruitOneOf0(
      parentReflectionGetter: instanceGetter,
      classReflection: AppleReflection.instance,
    ),
    
    oneOf1Part: FruitOneOf1(
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

  
  
  
  final FruitOneOf0 oneOf0Part;
  
  final FruitOneOf1 oneOf1Part;
  
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
  Fruit example({AggregatedDiscriminatorsResult? discriminators, Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>>
        discriminatorExampleResults = const {},}) {
    final _reflection = this;
    final actualDiscriminators = discriminators ?? _reflection.aggregatedDiscriminators;
    discriminatorExampleResults = Map.from(discriminatorExampleResults);
    for (final MapEntry(key: propName, value: mappings) in actualDiscriminators.entries) {
      if (discriminatorExampleResults.containsKey(propName)) {
        continue;
      }
      final r =  exampleDiscriminator(mappings);
      if (r != null){
        discriminatorExampleResults[propName] = r;
      }
    }

    final exampleResult = Fruit(
      color: () {
        var result = 


            
            


    
    exampleString()


;
        final preSelectedResult = discriminatorExampleResults[colorPart.oasName]?.key.key;
        if (preSelectedResult != null) {
          result = preSelectedResult;
        }
        return UndefinedWrapper(result);
      } (),
      
    );
    
    exampleResult.oneOf0 = oneOf0Part.example(discriminators: actualDiscriminators, discriminatorExampleResults: discriminatorExampleResults);
    
    exampleResult.oneOf1 = oneOf1Part.example(discriminators: actualDiscriminators, discriminatorExampleResults: discriminatorExampleResults);
    
    return exampleResult;
  }
}


class FruitOneOf0 extends OneOfReflection<Fruit, 
            Apple
> {
  const FruitOneOf0({
    super.classReflection,
    required FruitReflection Function() super.parentReflectionGetter,
    super.itemsReflection,
  });

  UndefinedWrapper<
            Apple
> example({required AggregatedDiscriminatorsResult discriminators, required Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>> discriminatorExampleResults}) {
    if (discriminatorExampleResults.isEmpty) {
      //return undefined for non-first oneOfs.
      // An example SHOULD be generated
    } else {
      // if this reflection wasn't a result of any property, don't generate an example.

      if (!discriminatorExampleResults.values
          .any((e) => e.value == classReflection)) {
        // if there are no discriminator examples targetting the current class:
        return UndefinedWrapper.undefined();
      } else {
        // An example SHOULD be generated
      }
    }
    return UndefinedWrapper(
            
            


    AppleReflection.instance.example(discriminators: discriminators, discriminatorExampleResults: discriminatorExampleResults)
    
);
  }
}

class FruitOneOf1 extends OneOfReflection<Fruit, 
            Banana
> {
  const FruitOneOf1({
    super.classReflection,
    required FruitReflection Function() super.parentReflectionGetter,
    super.itemsReflection,
  });

  UndefinedWrapper<
            Banana
> example({required AggregatedDiscriminatorsResult discriminators, required Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>> discriminatorExampleResults}) {
    if (discriminatorExampleResults.isEmpty) {
      //return undefined for non-first oneOfs.
      return UndefinedWrapper.undefined();
    } else {
      // if this reflection wasn't a result of any property, don't generate an example.

      if (!discriminatorExampleResults.values
          .any((e) => e.value == classReflection)) {
        // if there are no discriminator examples targetting the current class:
        return UndefinedWrapper.undefined();
      } else {
        // An example SHOULD be generated
      }
    }
    return UndefinedWrapper(
            
            


    BananaReflection.instance.example(discriminators: discriminators, discriminatorExampleResults: discriminatorExampleResults)
    
);
  }
}

class FruitXmlReflection {
    const FruitXmlReflection();
}

