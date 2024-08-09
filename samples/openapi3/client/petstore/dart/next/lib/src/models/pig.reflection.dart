// Model reflection

part of 'pig.dart';


//class reflection

class PigReflection extends ClassReflection<Pig> {
  static PigReflection instanceGetter() => instance;
  static const instance = PigReflection._(
    modelName: r'Pig',
    className: r'Pig',
    classNamePart: PropertyReflection<Pig, 
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
      r'BasquePig': BasquePigReflection.instance,
      r'DanishPig': DanishPigReflection.instance,
    },
    discriminatorMappings: const {
      r'BasquePig': BasquePigReflection.instance,
      r'DanishPig': DanishPigReflection.instance,
    },
    
    
    oneOf0Part: PigOneOf0(
      parentReflectionGetter: instanceGetter,
      classReflection: BasquePigReflection.instance,
    ),
    
    oneOf1Part: PigOneOf1(
      parentReflectionGetter: instanceGetter,
      classReflection: DanishPigReflection.instance,
    ),
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<Pig, Object

?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const PigReflection._({
    required this.modelName,
    required this.className,
    required this.classNamePart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.oneOf0Part,
    
    required this.oneOf1Part,
    
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<Pig, 
            String

> classNamePart;
  static 
            String

 _classNameGetter(Pig parent) {
    return parent.className;
  }
  static void _classNameSetter(Pig parent, 
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
  List<PropertyReflection<Pig, dynamic>> get properties => [
    classNamePart,
  ];

  final AdditionalPropertiesReflection<Pig, Object

?> additionalPropertiesPart;

  
  
  final PigOneOf0 oneOf0Part;
  
  final PigOneOf1 oneOf1Part;
  
  @override
  List<PartReflection<Pig, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<Pig, dynamic>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<Pig, dynamic>> get oneOfs => [
    oneOf0Part,oneOf1Part,
  ];
  @override
  List<AnyOfReflection<Pig, dynamic>> get anyOfs => [
    
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => Pig.canDeserialize(src);
  @override
  Pig Function(Object? src) get deserializeFunction =>
      (src) => Pig.deserialize(src);

  @override
  Object? Function(Pig src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of Pig.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  Pig example({AggregatedDiscriminatorsResult? discriminators, Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>>
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

    final exampleResult = Pig(
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
    
    return exampleResult;
  }
}


class PigOneOf0 extends OneOfReflection<Pig, 
            BasquePig
> {
  const PigOneOf0({
    super.classReflection,
    required PigReflection Function() super.parentReflectionGetter,
    super.itemsReflection,
  });

  UndefinedWrapper<
            BasquePig
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
            
            


    BasquePigReflection.instance.example(discriminators: discriminators, discriminatorExampleResults: discriminatorExampleResults)
    
);
  }
}

class PigOneOf1 extends OneOfReflection<Pig, 
            DanishPig
> {
  const PigOneOf1({
    super.classReflection,
    required PigReflection Function() super.parentReflectionGetter,
    super.itemsReflection,
  });

  UndefinedWrapper<
            DanishPig
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
            
            


    DanishPigReflection.instance.example(discriminators: discriminators, discriminatorExampleResults: discriminatorExampleResults)
    
);
  }
}

class PigXmlReflection {
    const PigXmlReflection();
}

