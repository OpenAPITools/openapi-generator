// Model reflection

part of 'enum_arrays.dart';


//class reflection

class EnumArraysReflection extends ClassReflection<EnumArrays> {
  static EnumArraysReflection instanceGetter() => instance;
  static const instance = EnumArraysReflection._(
    modelName: r'EnumArrays',
    className: r'EnumArrays',
    justSymbolPart: PropertyReflection<EnumArrays, UndefinedWrapper<
            EnumArraysJustSymbolEnum

>>(
      dartName: r'justSymbol',
      nullable: false,
      required: false,
      oasName: r'just_symbol',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _justSymbolGetter,
      setter: _justSymbolSetter,
    ),
    arrayEnumPart: PropertyReflection<EnumArrays, UndefinedWrapper<
    List<
        
            EnumArraysArrayEnumEnum

>

>>(
      dartName: r'arrayEnum',
      nullable: false,
      required: false,
      oasName: r'array_enum',
      oasType: r'array',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      itemsReflection: ItemsReflection<EnumArrays, 
            EnumArraysArrayEnumEnum

>(parentReflectionGetter: instanceGetter,),
      getter: _arrayEnumGetter,
      setter: _arrayEnumSetter,
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<EnumArrays, Object

?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const EnumArraysReflection._({
    required this.modelName,
    required this.className,
    required this.justSymbolPart,
    required this.arrayEnumPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<EnumArrays, UndefinedWrapper<
            EnumArraysJustSymbolEnum

>> justSymbolPart;
  static UndefinedWrapper<
            EnumArraysJustSymbolEnum

> _justSymbolGetter(EnumArrays parent) {
    return parent.justSymbol;
  }
  static void _justSymbolSetter(EnumArrays parent, UndefinedWrapper<
            EnumArraysJustSymbolEnum

> value) {
    parent.justSymbol = value;
  }
  final PropertyReflection<EnumArrays, UndefinedWrapper<
    List<
        
            EnumArraysArrayEnumEnum

>

>> arrayEnumPart;
  static UndefinedWrapper<
    List<
        
            EnumArraysArrayEnumEnum

>

> _arrayEnumGetter(EnumArrays parent) {
    return parent.arrayEnum;
  }
  static void _arrayEnumSetter(EnumArrays parent, UndefinedWrapper<
    List<
        
            EnumArraysArrayEnumEnum

>

> value) {
    parent.arrayEnum = value;
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
  List<PropertyReflection<EnumArrays, dynamic>> get properties => [
    justSymbolPart,
arrayEnumPart,
  ];

  final AdditionalPropertiesReflection<EnumArrays, Object

?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<EnumArrays, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<EnumArrays, dynamic>> get allOfs => [
    
  ];


  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => EnumArrays.canDeserialize(src);
  @override
  EnumArrays Function(Object? src) get deserializeFunction =>
      (src) => EnumArrays.deserialize(src);

  @override
  Object? Function(EnumArrays src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of EnumArrays.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  EnumArrays example({AggregatedDiscriminatorsResult? discriminators, Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>>
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

    final exampleResult = EnumArrays(
      justSymbol: () {
        var result = 


            exampleEnum(EnumArraysJustSymbolEnum.values)



;
        return UndefinedWrapper(result);
      } (),
      arrayEnum: () {
        var result = 


    exampleList(() { return 


            exampleEnum(EnumArraysArrayEnumEnum.values)



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


class EnumArraysXmlReflection {
    const EnumArraysXmlReflection();
}

