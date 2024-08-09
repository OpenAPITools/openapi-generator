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
    
    
    anyOf0Part: GmFruitAnyOf0(
      parentReflectionGetter: instanceGetter,
      classReflection: AppleReflection.instance,
    ),
    
    anyOf1Part: GmFruitAnyOf1(
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

  
  
  
  final GmFruitAnyOf0 anyOf0Part;
  
  final GmFruitAnyOf1 anyOf1Part;
  
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
  GmFruit example({AggregatedDiscriminatorsResult? discriminators, Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>>
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

    final exampleResult = GmFruit(
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
    
    exampleResult.anyOf0 = anyOf0Part.example(discriminators: actualDiscriminators, discriminatorExampleResults: discriminatorExampleResults);
    
    exampleResult.anyOf1 = anyOf1Part.example(discriminators: actualDiscriminators, discriminatorExampleResults: discriminatorExampleResults);
    
    return exampleResult;
  }
}

class GmFruitAnyOf0 extends AnyOfReflection<GmFruit, 
            Apple
> {
  const GmFruitAnyOf0({
    super.classReflection,
    required GmFruitReflection Function() super.parentReflectionGetter,
    super.itemsReflection,
  });

  UndefinedWrapper<
            Apple
> example({required AggregatedDiscriminatorsResult discriminators, required Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>> discriminatorExampleResults}) {
    if (discriminatorExampleResults.isNotEmpty) {
      if (!discriminatorExampleResults.values
          .any((e) => e.value == classReflection)) {
        return UndefinedWrapper.undefined();
      }
    }
    return UndefinedWrapper(
            
            


    AppleReflection.instance.example(discriminators: discriminators, discriminatorExampleResults: discriminatorExampleResults)
    
);
  }
}
class GmFruitAnyOf1 extends AnyOfReflection<GmFruit, 
            Banana
> {
  const GmFruitAnyOf1({
    super.classReflection,
    required GmFruitReflection Function() super.parentReflectionGetter,
    super.itemsReflection,
  });

  UndefinedWrapper<
            Banana
> example({required AggregatedDiscriminatorsResult discriminators, required Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>> discriminatorExampleResults}) {
    if (discriminatorExampleResults.isNotEmpty) {
      if (!discriminatorExampleResults.values
          .any((e) => e.value == classReflection)) {
        return UndefinedWrapper.undefined();
      }
    }
    return UndefinedWrapper(
            
            


    BananaReflection.instance.example(discriminators: discriminators, discriminatorExampleResults: discriminatorExampleResults)
    
);
  }
}

class GmFruitXmlReflection {
    const GmFruitXmlReflection();
}

