// Model reflection

part of 'has_only_read_only.dart';


//class reflection

class HasOnlyReadOnlyReflection extends ClassReflection<HasOnlyReadOnly> {
  static HasOnlyReadOnlyReflection instanceGetter() => instance;
  static const instance = HasOnlyReadOnlyReflection._(
    modelName: r'hasOnlyReadOnly',
    className: r'HasOnlyReadOnly',
    barPart: PropertyReflection<HasOnlyReadOnly, UndefinedWrapper<
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
    fooPart: PropertyReflection<HasOnlyReadOnly, UndefinedWrapper<
            String

>>(
      dartName: r'foo',
      nullable: false,
      required: false,
      oasName: r'foo',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _fooGetter,
      setter: _fooSetter,
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<HasOnlyReadOnly, Object

?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const HasOnlyReadOnlyReflection._({
    required this.modelName,
    required this.className,
    required this.barPart,
    required this.fooPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<HasOnlyReadOnly, UndefinedWrapper<
            String

>> barPart;
  static UndefinedWrapper<
            String

> _barGetter(HasOnlyReadOnly parent) {
    return parent.bar;
  }
  static void _barSetter(HasOnlyReadOnly parent, UndefinedWrapper<
            String

> value) {
    parent.bar = value;
  }
  final PropertyReflection<HasOnlyReadOnly, UndefinedWrapper<
            String

>> fooPart;
  static UndefinedWrapper<
            String

> _fooGetter(HasOnlyReadOnly parent) {
    return parent.foo;
  }
  static void _fooSetter(HasOnlyReadOnly parent, UndefinedWrapper<
            String

> value) {
    parent.foo = value;
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
  List<PropertyReflection<HasOnlyReadOnly, dynamic>> get properties => [
    barPart,
fooPart,
  ];

  final AdditionalPropertiesReflection<HasOnlyReadOnly, Object

?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<HasOnlyReadOnly, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<HasOnlyReadOnly, dynamic>> get allOfs => [
    
  ];


  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => HasOnlyReadOnly.canDeserialize(src);
  @override
  HasOnlyReadOnly Function(Object? src) get deserializeFunction =>
      (src) => HasOnlyReadOnly.deserialize(src);

  @override
  Object? Function(HasOnlyReadOnly src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of HasOnlyReadOnly.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  HasOnlyReadOnly example({AggregatedDiscriminatorsResult? discriminators, Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>>
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

    final exampleResult = HasOnlyReadOnly(
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
      foo: () {
        var result = 


            
            


    
    exampleString()


;
        final preSelectedResult = discriminatorExampleResults[fooPart.oasName]?.key.key;
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


class HasOnlyReadOnlyXmlReflection {
    const HasOnlyReadOnlyXmlReflection();
}

