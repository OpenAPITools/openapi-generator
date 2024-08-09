// Model reflection

part of 'tag.dart';


//class reflection

class TagReflection extends ClassReflection<Tag> {
  static TagReflection instanceGetter() => instance;
  static const instance = TagReflection._(
    modelName: r'Tag',
    className: r'Tag',
    idPart: PropertyReflection<Tag, UndefinedWrapper<
            int

>>(
      dartName: r'id',
      nullable: false,
      required: false,
      oasName: r'id',
      oasType: r'integer',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _idGetter,
      setter: _idSetter,
    ),
    namePart: PropertyReflection<Tag, UndefinedWrapper<
            String

>>(
      dartName: r'name',
      nullable: false,
      required: false,
      oasName: r'name',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _nameGetter,
      setter: _nameSetter,
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<Tag, Object

?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const TagReflection._({
    required this.modelName,
    required this.className,
    required this.idPart,
    required this.namePart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<Tag, UndefinedWrapper<
            int

>> idPart;
  static UndefinedWrapper<
            int

> _idGetter(Tag parent) {
    return parent.id;
  }
  static void _idSetter(Tag parent, UndefinedWrapper<
            int

> value) {
    parent.id = value;
  }
  final PropertyReflection<Tag, UndefinedWrapper<
            String

>> namePart;
  static UndefinedWrapper<
            String

> _nameGetter(Tag parent) {
    return parent.name;
  }
  static void _nameSetter(Tag parent, UndefinedWrapper<
            String

> value) {
    parent.name = value;
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
  List<PropertyReflection<Tag, dynamic>> get properties => [
    idPart,
namePart,
  ];

  final AdditionalPropertiesReflection<Tag, Object

?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<Tag, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<Tag, dynamic>> get allOfs => [
    
  ];


  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => Tag.canDeserialize(src);
  @override
  Tag Function(Object? src) get deserializeFunction =>
      (src) => Tag.deserialize(src);

  @override
  Object? Function(Tag src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of Tag.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  Tag example({AggregatedDiscriminatorsResult? discriminators, Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>>
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

    final exampleResult = Tag(
      id: () {
        var result = 


            
            


    
    exampleint()


;
        return UndefinedWrapper(result);
      } (),
      name: () {
        var result = 


            
            


    
    exampleString()


;
        final preSelectedResult = discriminatorExampleResults[namePart.oasName]?.key.key;
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


class TagXmlReflection {
    const TagXmlReflection();
}

