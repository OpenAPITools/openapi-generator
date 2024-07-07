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
  Foo example({Map<String, (ClassReflection, PropertyReflection)> discriminators = const {}}) {
    final _reflection = this;
    if (discriminators.isEmpty) discriminators = _reflection.aggregatedDiscriminators;
    return Foo(
      bar: () {
        PartReflection? _partReflection = _reflection.barPart;
        
        final disc = discriminators[r'bar'];
        if (disc != null) {
          final result = exampleDiscriminator(_partReflection, disc);
          if (result != null) {
            return UndefinedWrapper(result);
          }
        }
        
        return UndefinedWrapper(


            
            


    
    exampleString()


);
      }(),
      additionalProperties: () { PartReflection? _partReflection = _reflection.additionalPropertiesPart; return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
      
    );
  }
}

class FooXmlReflection {
    const FooXmlReflection();
}

