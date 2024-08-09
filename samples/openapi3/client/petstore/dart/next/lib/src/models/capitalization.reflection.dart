// Model reflection

part of 'capitalization.dart';


//class reflection

class CapitalizationReflection extends ClassReflection<Capitalization> {
  static CapitalizationReflection instanceGetter() => instance;
  static const instance = CapitalizationReflection._(
    modelName: r'Capitalization',
    className: r'Capitalization',
    smallCamelPart: PropertyReflection<Capitalization, UndefinedWrapper<
            String

>>(
      dartName: r'smallCamel',
      nullable: false,
      required: false,
      oasName: r'smallCamel',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _smallCamelGetter,
      setter: _smallCamelSetter,
    ),
    capitalCamelPart: PropertyReflection<Capitalization, UndefinedWrapper<
            String

>>(
      dartName: r'capitalCamel',
      nullable: false,
      required: false,
      oasName: r'CapitalCamel',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _capitalCamelGetter,
      setter: _capitalCamelSetter,
    ),
    smallSnakePart: PropertyReflection<Capitalization, UndefinedWrapper<
            String

>>(
      dartName: r'smallSnake',
      nullable: false,
      required: false,
      oasName: r'small_Snake',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _smallSnakeGetter,
      setter: _smallSnakeSetter,
    ),
    capitalSnakePart: PropertyReflection<Capitalization, UndefinedWrapper<
            String

>>(
      dartName: r'capitalSnake',
      nullable: false,
      required: false,
      oasName: r'Capital_Snake',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _capitalSnakeGetter,
      setter: _capitalSnakeSetter,
    ),
    scAETHFlowPointsPart: PropertyReflection<Capitalization, UndefinedWrapper<
            String

>>(
      dartName: r'scAETHFlowPoints',
      nullable: false,
      required: false,
      oasName: r'SCA_ETH_Flow_Points',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _scAETHFlowPointsGetter,
      setter: _scAETHFlowPointsSetter,
    ),
    ATT_NAMEPart: PropertyReflection<Capitalization, UndefinedWrapper<
            String

>>(
      dartName: r'ATT_NAME',
      nullable: false,
      required: false,
      oasName: r'ATT_NAME',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _ATT_NAMEGetter,
      setter: _ATT_NAMESetter,
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<Capitalization, Object

?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const CapitalizationReflection._({
    required this.modelName,
    required this.className,
    required this.smallCamelPart,
    required this.capitalCamelPart,
    required this.smallSnakePart,
    required this.capitalSnakePart,
    required this.scAETHFlowPointsPart,
    required this.ATT_NAMEPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<Capitalization, UndefinedWrapper<
            String

>> smallCamelPart;
  static UndefinedWrapper<
            String

> _smallCamelGetter(Capitalization parent) {
    return parent.smallCamel;
  }
  static void _smallCamelSetter(Capitalization parent, UndefinedWrapper<
            String

> value) {
    parent.smallCamel = value;
  }
  final PropertyReflection<Capitalization, UndefinedWrapper<
            String

>> capitalCamelPart;
  static UndefinedWrapper<
            String

> _capitalCamelGetter(Capitalization parent) {
    return parent.capitalCamel;
  }
  static void _capitalCamelSetter(Capitalization parent, UndefinedWrapper<
            String

> value) {
    parent.capitalCamel = value;
  }
  final PropertyReflection<Capitalization, UndefinedWrapper<
            String

>> smallSnakePart;
  static UndefinedWrapper<
            String

> _smallSnakeGetter(Capitalization parent) {
    return parent.smallSnake;
  }
  static void _smallSnakeSetter(Capitalization parent, UndefinedWrapper<
            String

> value) {
    parent.smallSnake = value;
  }
  final PropertyReflection<Capitalization, UndefinedWrapper<
            String

>> capitalSnakePart;
  static UndefinedWrapper<
            String

> _capitalSnakeGetter(Capitalization parent) {
    return parent.capitalSnake;
  }
  static void _capitalSnakeSetter(Capitalization parent, UndefinedWrapper<
            String

> value) {
    parent.capitalSnake = value;
  }
  final PropertyReflection<Capitalization, UndefinedWrapper<
            String

>> scAETHFlowPointsPart;
  static UndefinedWrapper<
            String

> _scAETHFlowPointsGetter(Capitalization parent) {
    return parent.scAETHFlowPoints;
  }
  static void _scAETHFlowPointsSetter(Capitalization parent, UndefinedWrapper<
            String

> value) {
    parent.scAETHFlowPoints = value;
  }
  final PropertyReflection<Capitalization, UndefinedWrapper<
            String

>> ATT_NAMEPart;
  static UndefinedWrapper<
            String

> _ATT_NAMEGetter(Capitalization parent) {
    return parent.ATT_NAME;
  }
  static void _ATT_NAMESetter(Capitalization parent, UndefinedWrapper<
            String

> value) {
    parent.ATT_NAME = value;
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
  List<PropertyReflection<Capitalization, dynamic>> get properties => [
    smallCamelPart,
capitalCamelPart,
smallSnakePart,
capitalSnakePart,
scAETHFlowPointsPart,
ATT_NAMEPart,
  ];

  final AdditionalPropertiesReflection<Capitalization, Object

?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<Capitalization, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<Capitalization, dynamic>> get allOfs => [
    
  ];


  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => Capitalization.canDeserialize(src);
  @override
  Capitalization Function(Object? src) get deserializeFunction =>
      (src) => Capitalization.deserialize(src);

  @override
  Object? Function(Capitalization src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of Capitalization.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  Capitalization example({AggregatedDiscriminatorsResult? discriminators, Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>>
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

    final exampleResult = Capitalization(
      smallCamel: () {
        var result = 


            
            


    
    exampleString()


;
        final preSelectedResult = discriminatorExampleResults[smallCamelPart.oasName]?.key.key;
        if (preSelectedResult != null) {
          result = preSelectedResult;
        }
        return UndefinedWrapper(result);
      } (),
      capitalCamel: () {
        var result = 


            
            


    
    exampleString()


;
        final preSelectedResult = discriminatorExampleResults[capitalCamelPart.oasName]?.key.key;
        if (preSelectedResult != null) {
          result = preSelectedResult;
        }
        return UndefinedWrapper(result);
      } (),
      smallSnake: () {
        var result = 


            
            


    
    exampleString()


;
        final preSelectedResult = discriminatorExampleResults[smallSnakePart.oasName]?.key.key;
        if (preSelectedResult != null) {
          result = preSelectedResult;
        }
        return UndefinedWrapper(result);
      } (),
      capitalSnake: () {
        var result = 


            
            


    
    exampleString()


;
        final preSelectedResult = discriminatorExampleResults[capitalSnakePart.oasName]?.key.key;
        if (preSelectedResult != null) {
          result = preSelectedResult;
        }
        return UndefinedWrapper(result);
      } (),
      scAETHFlowPoints: () {
        var result = 


            
            


    
    exampleString()


;
        final preSelectedResult = discriminatorExampleResults[scAETHFlowPointsPart.oasName]?.key.key;
        if (preSelectedResult != null) {
          result = preSelectedResult;
        }
        return UndefinedWrapper(result);
      } (),
      ATT_NAME: () {
        var result = 


            
            


    
    exampleString()


;
        final preSelectedResult = discriminatorExampleResults[ATT_NAMEPart.oasName]?.key.key;
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


class CapitalizationXmlReflection {
    const CapitalizationXmlReflection();
}

