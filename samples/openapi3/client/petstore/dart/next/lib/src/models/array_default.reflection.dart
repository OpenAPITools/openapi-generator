// Model reflection

part of 'array_default.dart';


//class reflection

class ArrayDefaultReflection extends ClassReflection<ArrayDefault> {
  static ArrayDefaultReflection instanceGetter() => instance;
  static const instance = ArrayDefaultReflection._(
    modelName: r'ArrayDefault',
    className: r'ArrayDefault',
    withDefaultEmptyBracketPart: PropertyReflection<ArrayDefault, UndefinedWrapper<
    List<
        
            String

>

>>(
      dartName: r'withDefaultEmptyBracket',
      nullable: false,
      required: false,
      oasName: r'WithDefaultEmptyBracket',
      oasType: r'array',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      itemsReflection: ItemsReflection<ArrayDefault, 
            String

>(parentReflectionGetter: instanceGetter,),
      getter: _withDefaultEmptyBracketGetter,
      setter: _withDefaultEmptyBracketSetter,
    ),
    withoutDefaultPart: PropertyReflection<ArrayDefault, UndefinedWrapper<
    List<
        
            String

>

>>(
      dartName: r'withoutDefault',
      nullable: false,
      required: false,
      oasName: r'WithoutDefault',
      oasType: r'array',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      itemsReflection: ItemsReflection<ArrayDefault, 
            String

>(parentReflectionGetter: instanceGetter,),
      getter: _withoutDefaultGetter,
      setter: _withoutDefaultSetter,
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<ArrayDefault, Object

?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const ArrayDefaultReflection._({
    required this.modelName,
    required this.className,
    required this.withDefaultEmptyBracketPart,
    required this.withoutDefaultPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<ArrayDefault, UndefinedWrapper<
    List<
        
            String

>

>> withDefaultEmptyBracketPart;
  static UndefinedWrapper<
    List<
        
            String

>

> _withDefaultEmptyBracketGetter(ArrayDefault parent) {
    return parent.withDefaultEmptyBracket;
  }
  static void _withDefaultEmptyBracketSetter(ArrayDefault parent, UndefinedWrapper<
    List<
        
            String

>

> value) {
    parent.withDefaultEmptyBracket = value;
  }
  final PropertyReflection<ArrayDefault, UndefinedWrapper<
    List<
        
            String

>

>> withoutDefaultPart;
  static UndefinedWrapper<
    List<
        
            String

>

> _withoutDefaultGetter(ArrayDefault parent) {
    return parent.withoutDefault;
  }
  static void _withoutDefaultSetter(ArrayDefault parent, UndefinedWrapper<
    List<
        
            String

>

> value) {
    parent.withoutDefault = value;
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
  List<PropertyReflection<ArrayDefault, dynamic>> get properties => [
    withDefaultEmptyBracketPart,
withoutDefaultPart,
  ];

  final AdditionalPropertiesReflection<ArrayDefault, Object

?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<ArrayDefault, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<ArrayDefault, dynamic>> get allOfs => [
    
  ];


  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => ArrayDefault.canDeserialize(src);
  @override
  ArrayDefault Function(Object? src) get deserializeFunction =>
      (src) => ArrayDefault.deserialize(src);

  @override
  Object? Function(ArrayDefault src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of ArrayDefault.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  ArrayDefault example({AggregatedDiscriminatorsResult? discriminators, Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>>
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

    final exampleResult = ArrayDefault(
      withDefaultEmptyBracket: () {
        var result = 


    exampleList(() { return 


            
            


    
    exampleString()


; })



;
        return UndefinedWrapper(result);
      } (),
      withoutDefault: () {
        var result = 


    exampleList(() { return 


            
            


    
    exampleString()


; })



;
        return UndefinedWrapper(result);
      } (),
      additionalProperties: () { return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
    );
    
    return exampleResult;
  }
}


class ArrayDefaultXmlReflection {
    const ArrayDefaultXmlReflection();
}

