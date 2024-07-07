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
  ReadOnlyFirst example({Map<String, (ClassReflection, PropertyReflection)> discriminators = const {}}) {
    final _reflection = this;
    if (discriminators.isEmpty) discriminators = _reflection.aggregatedDiscriminators;
    return ReadOnlyFirst(
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
      baz: () {
        PartReflection? _partReflection = _reflection.bazPart;
        
        final disc = discriminators[r'baz'];
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

class ReadOnlyFirstXmlReflection {
    const ReadOnlyFirstXmlReflection();
}

