// Model reflection

part of 'foo.dart';


//class reflection

class FooReflection extends ClassReflection<Foo> {
  static FooReflection instanceGetter() => instance;
  static const instance = FooReflection._(
    modelName: r'Foo',
    className: r'Foo',
    barPart: PropertyReflection<Foo, UndefinedWrapper<
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
    
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<Foo, Object

?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const FooReflection._({
    required this.modelName,
    required this.className,
    required this.barPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<Foo, UndefinedWrapper<
            String

>> barPart;
  static UndefinedWrapper<
            String

> _barGetter(Foo parent) {
    return parent.bar;
  }
  static void _barSetter(Foo parent, UndefinedWrapper<
            String

> value) {
    parent.bar = value;
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
  List<PropertyReflection<Foo, dynamic>> get properties => [
    barPart,
  ];

  final AdditionalPropertiesReflection<Foo, Object

?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<Foo, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<Foo, dynamic>> get allOfs => [
    
  ];


  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => Foo.canDeserialize(src);
  @override
  Foo Function(Object? src) get deserializeFunction =>
      (src) => Foo.deserialize(src);

  @override
  Object? Function(Foo src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of Foo.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  Foo example({AggregatedDiscriminatorsResult? discriminators, Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>>
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

    final exampleResult = Foo(
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
      additionalProperties: () { return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
    );
    
    return exampleResult;
  }
}


class FooXmlReflection {
    const FooXmlReflection();
}

