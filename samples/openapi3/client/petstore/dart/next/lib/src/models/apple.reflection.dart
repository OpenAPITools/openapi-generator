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
  Apple example({AggregatedDiscriminatorsResult? discriminators, Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>>
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

    final exampleResult = Apple(
      cultivar: () {
        var result = 


            
            


    
    exampleString()


;
        final preSelectedResult = discriminatorExampleResults[cultivarPart.oasName]?.key.key;
        if (preSelectedResult != null) {
          result = preSelectedResult;
        }
        return UndefinedWrapper(result);
      } (),
      origin: () {
        var result = 


            
            


    
    exampleString()


;
        final preSelectedResult = discriminatorExampleResults[originPart.oasName]?.key.key;
        if (preSelectedResult != null) {
          result = preSelectedResult;
        }
        return UndefinedWrapper(result);
      } (),
      additionalProperties: () { return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
    );
    
    return exampleResult;
  }
}


class AppleXmlReflection {
    const AppleXmlReflection();
}

