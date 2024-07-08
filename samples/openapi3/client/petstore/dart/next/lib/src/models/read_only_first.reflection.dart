// Model reflection

part of 'read_only_first.dart';


//class reflection

class ReadOnlyFirstReflection extends ClassReflection<ReadOnlyFirst> {
  static ReadOnlyFirstReflection instanceGetter() => instance;
  static const instance = ReadOnlyFirstReflection._(
    modelName: r'ReadOnlyFirst',
    className: r'ReadOnlyFirst',
    barPart: PropertyReflection<ReadOnlyFirst, UndefinedWrapper<
            String

>>(
      dartName: r'bar',
      nullable: false,
      required: false,
      oasName: r'bar',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _barGetter,
      setter: _barSetter,
    ),
    bazPart: PropertyReflection<ReadOnlyFirst, UndefinedWrapper<
            String

>>(
      dartName: r'baz',
      nullable: false,
      required: false,
      oasName: r'baz',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _bazGetter,
      setter: _bazSetter,
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<ReadOnlyFirst, Object

?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const ReadOnlyFirstReflection._({
    required this.modelName,
    required this.className,
    required this.barPart,
    required this.bazPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<ReadOnlyFirst, UndefinedWrapper<
            String

>> barPart;
  static UndefinedWrapper<
            String

> _barGetter(ReadOnlyFirst parent) {
    return parent.bar;
  }
  static void _barSetter(ReadOnlyFirst parent, UndefinedWrapper<
            String

> value) {
    parent.bar = value;
  }
  final PropertyReflection<ReadOnlyFirst, UndefinedWrapper<
            String

>> bazPart;
  static UndefinedWrapper<
            String

> _bazGetter(ReadOnlyFirst parent) {
    return parent.baz;
  }
  static void _bazSetter(ReadOnlyFirst parent, UndefinedWrapper<
            String

> value) {
    parent.baz = value;
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
  List<PropertyReflection<ReadOnlyFirst, dynamic>> get properties => [
    barPart,
bazPart,
  ];

  final AdditionalPropertiesReflection<ReadOnlyFirst, Object

?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<ReadOnlyFirst, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<ReadOnlyFirst, dynamic>> get allOfs => [
    
  ];


  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => ReadOnlyFirst.canDeserialize(src);
  @override
  ReadOnlyFirst Function(Object? src) get deserializeFunction =>
      (src) => ReadOnlyFirst.deserialize(src);

  @override
  Object? Function(ReadOnlyFirst src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of ReadOnlyFirst.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  ReadOnlyFirst example({AggregatedDiscriminatorsResult? discriminators, Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>>
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

    final exampleResult = ReadOnlyFirst(
      bar: () {
        var result = 


            
            


    
    exampleString()


;
        final preSelectedResult = discriminatorExampleResults[barPart.oasName]?.key.key;
        if (preSelectedResult != null) {
          result = preSelectedResult;
        }
        return UndefinedWrapper(result);
      } (),
      baz: () {
        var result = 


            
            


    
    exampleString()


;
        final preSelectedResult = discriminatorExampleResults[bazPart.oasName]?.key.key;
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


class ReadOnlyFirstXmlReflection {
    const ReadOnlyFirstXmlReflection();
}

