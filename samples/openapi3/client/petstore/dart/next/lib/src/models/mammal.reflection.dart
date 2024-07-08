// Model reflection

part of 'mammal.dart';


//class reflection

class MammalReflection extends ClassReflection<Mammal> {
  static MammalReflection instanceGetter() => instance;
  static const instance = MammalReflection._(
    modelName: r'mammal',
    className: r'Mammal',
    classNamePart: PropertyReflection<Mammal, 
            String

>(
      dartName: r'className',
      nullable: false,
      required: true,
      oasName: r'className',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: true,
      getter: _classNameGetter,
      setter: _classNameSetter,
    ),
    discriminatorKey: r'className',
    discriminatorImplicitMappings: const {
      r'Pig': PigReflection.instance,
      r'Whale': WhaleReflection.instance,
      r'Zebra': ZebraReflection.instance,
    },
    discriminatorMappings: const {
      r'Pig': PigReflection.instance,
      r'whale': WhaleReflection.instance,
      r'zebra': ZebraReflection.instance,
    },
    
    
    oneOf0Part: MammalOneOf0(
      parentReflectionGetter: instanceGetter,
      classReflection: WhaleReflection.instance,
    ),
    
    oneOf1Part: MammalOneOf1(
      parentReflectionGetter: instanceGetter,
      classReflection: ZebraReflection.instance,
    ),
    
    oneOf2Part: MammalOneOf2(
      parentReflectionGetter: instanceGetter,
      classReflection: PigReflection.instance,
    ),
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<Mammal, Object

?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const MammalReflection._({
    required this.modelName,
    required this.className,
    required this.classNamePart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.oneOf0Part,
    
    required this.oneOf1Part,
    
    required this.oneOf2Part,
    
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<Mammal, 
            String

> classNamePart;
  static 
            String

 _classNameGetter(Mammal parent) {
    return parent.className;
  }
  static void _classNameSetter(Mammal parent, 
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
  List<PropertyReflection<Mammal, dynamic>> get properties => [
    classNamePart,
  ];

  final AdditionalPropertiesReflection<Mammal, Object

?> additionalPropertiesPart;

  
  
  final MammalOneOf0 oneOf0Part;
  
  final MammalOneOf1 oneOf1Part;
  
  final MammalOneOf2 oneOf2Part;
  
  @override
  List<PartReflection<Mammal, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<Mammal, dynamic>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<Mammal, dynamic>> get oneOfs => [
    oneOf0Part,oneOf1Part,oneOf2Part,
  ];
  @override
  List<AnyOfReflection<Mammal, dynamic>> get anyOfs => [
    
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => Mammal.canDeserialize(src);
  @override
  Mammal Function(Object? src) get deserializeFunction =>
      (src) => Mammal.deserialize(src);

  @override
  Object? Function(Mammal src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of Mammal.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  Mammal example({AggregatedDiscriminatorsResult? discriminators, Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>>
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

    final exampleResult = Mammal(
      className: () {
        var result = 


            
            


    
    exampleString()


;
        final preSelectedResult = discriminatorExampleResults[classNamePart.oasName]?.key.key;
        if (preSelectedResult != null) {
          result = preSelectedResult;
        }
        return result;
      } (),
      additionalProperties: () { return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
    );
    
    exampleResult.oneOf0 = oneOf0Part.example(discriminators: actualDiscriminators, discriminatorExampleResults: discriminatorExampleResults);
    
    exampleResult.oneOf1 = oneOf1Part.example(discriminators: actualDiscriminators, discriminatorExampleResults: discriminatorExampleResults);
    
    exampleResult.oneOf2 = oneOf2Part.example(discriminators: actualDiscriminators, discriminatorExampleResults: discriminatorExampleResults);
    
    return exampleResult;
  }
}


class MammalOneOf0 extends OneOfReflection<Mammal, 
            Whale
> {
  const MammalOneOf0({
    super.classReflection,
    required MammalReflection Function() super.parentReflectionGetter,
    super.itemsReflection,
  });

  UndefinedWrapper<
            Whale
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
            
            


    WhaleReflection.instance.example(discriminators: discriminators, discriminatorExampleResults: discriminatorExampleResults)
    
);
  }
}

class MammalOneOf1 extends OneOfReflection<Mammal, 
            Zebra
> {
  const MammalOneOf1({
    super.classReflection,
    required MammalReflection Function() super.parentReflectionGetter,
    super.itemsReflection,
  });

  UndefinedWrapper<
            Zebra
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
            
            


    ZebraReflection.instance.example(discriminators: discriminators, discriminatorExampleResults: discriminatorExampleResults)
    
);
  }
}

class MammalOneOf2 extends OneOfReflection<Mammal, 
            Pig
> {
  const MammalOneOf2({
    super.classReflection,
    required MammalReflection Function() super.parentReflectionGetter,
    super.itemsReflection,
  });

  UndefinedWrapper<
            Pig
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
            
            


    PigReflection.instance.example(discriminators: discriminators, discriminatorExampleResults: discriminatorExampleResults)
    
);
  }
}

class MammalXmlReflection {
    const MammalXmlReflection();
}

