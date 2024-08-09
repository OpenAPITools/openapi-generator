// Model reflection

part of 'object_with_deprecated_fields.dart';


//class reflection

class ObjectWithDeprecatedFieldsReflection extends ClassReflection<ObjectWithDeprecatedFields> {
  static ObjectWithDeprecatedFieldsReflection instanceGetter() => instance;
  static const instance = ObjectWithDeprecatedFieldsReflection._(
    modelName: r'ObjectWithDeprecatedFields',
    className: r'ObjectWithDeprecatedFields',
    uuidPart: PropertyReflection<ObjectWithDeprecatedFields, UndefinedWrapper<
            String

>>(
      dartName: r'uuid',
      nullable: false,
      required: false,
      oasName: r'uuid',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _uuidGetter,
      setter: _uuidSetter,
    ),
    idPart: PropertyReflection<ObjectWithDeprecatedFields, UndefinedWrapper<
            num

>>(
      dartName: r'id',
      nullable: false,
      required: false,
      oasName: r'id',
      oasType: r'number',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _idGetter,
      setter: _idSetter,
    ),
    deprecatedRefPart: PropertyReflection<ObjectWithDeprecatedFields, UndefinedWrapper<
            DeprecatedObject

>>(
      dartName: r'deprecatedRef',
      nullable: false,
      required: false,
      oasName: r'deprecatedRef',
      oasType: r'DeprecatedObject',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      classReflection: DeprecatedObjectReflection.instance,
      getter: _deprecatedRefGetter,
      setter: _deprecatedRefSetter,
    ),
    barsPart: PropertyReflection<ObjectWithDeprecatedFields, UndefinedWrapper<
    List<
        
            String

>

>>(
      dartName: r'bars',
      nullable: false,
      required: false,
      oasName: r'bars',
      oasType: r'array',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      itemsReflection: ItemsReflection<ObjectWithDeprecatedFields, 
            String

>(parentReflectionGetter: instanceGetter,),
      getter: _barsGetter,
      setter: _barsSetter,
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<ObjectWithDeprecatedFields, Object

?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const ObjectWithDeprecatedFieldsReflection._({
    required this.modelName,
    required this.className,
    required this.uuidPart,
    required this.idPart,
    required this.deprecatedRefPart,
    required this.barsPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<ObjectWithDeprecatedFields, UndefinedWrapper<
            String

>> uuidPart;
  static UndefinedWrapper<
            String

> _uuidGetter(ObjectWithDeprecatedFields parent) {
    return parent.uuid;
  }
  static void _uuidSetter(ObjectWithDeprecatedFields parent, UndefinedWrapper<
            String

> value) {
    parent.uuid = value;
  }
  final PropertyReflection<ObjectWithDeprecatedFields, UndefinedWrapper<
            num

>> idPart;
  static UndefinedWrapper<
            num

> _idGetter(ObjectWithDeprecatedFields parent) {
    return parent.id;
  }
  static void _idSetter(ObjectWithDeprecatedFields parent, UndefinedWrapper<
            num

> value) {
    parent.id = value;
  }
  final PropertyReflection<ObjectWithDeprecatedFields, UndefinedWrapper<
            DeprecatedObject

>> deprecatedRefPart;
  static UndefinedWrapper<
            DeprecatedObject

> _deprecatedRefGetter(ObjectWithDeprecatedFields parent) {
    return parent.deprecatedRef;
  }
  static void _deprecatedRefSetter(ObjectWithDeprecatedFields parent, UndefinedWrapper<
            DeprecatedObject

> value) {
    parent.deprecatedRef = value;
  }
  final PropertyReflection<ObjectWithDeprecatedFields, UndefinedWrapper<
    List<
        
            String

>

>> barsPart;
  static UndefinedWrapper<
    List<
        
            String

>

> _barsGetter(ObjectWithDeprecatedFields parent) {
    return parent.bars;
  }
  static void _barsSetter(ObjectWithDeprecatedFields parent, UndefinedWrapper<
    List<
        
            String

>

> value) {
    parent.bars = value;
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
  List<PropertyReflection<ObjectWithDeprecatedFields, dynamic>> get properties => [
    uuidPart,
idPart,
deprecatedRefPart,
barsPart,
  ];

  final AdditionalPropertiesReflection<ObjectWithDeprecatedFields, Object

?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<ObjectWithDeprecatedFields, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<ObjectWithDeprecatedFields, dynamic>> get allOfs => [
    
  ];


  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => ObjectWithDeprecatedFields.canDeserialize(src);
  @override
  ObjectWithDeprecatedFields Function(Object? src) get deserializeFunction =>
      (src) => ObjectWithDeprecatedFields.deserialize(src);

  @override
  Object? Function(ObjectWithDeprecatedFields src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of ObjectWithDeprecatedFields.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  ObjectWithDeprecatedFields example({AggregatedDiscriminatorsResult? discriminators, Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>>
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

    final exampleResult = ObjectWithDeprecatedFields(
      uuid: () {
        var result = 


            
            


    
    exampleString()


;
        final preSelectedResult = discriminatorExampleResults[uuidPart.oasName]?.key.key;
        if (preSelectedResult != null) {
          result = preSelectedResult;
        }
        return UndefinedWrapper(result);
      } (),
      id: () {
        var result = 


            
            


    
    examplenum()


;
        return UndefinedWrapper(result);
      } (),
      deprecatedRef: () {
        var result = 


            
            


    DeprecatedObjectReflection.instance.example()
    


;
        return UndefinedWrapper(result);
      } (),
      bars: () {
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


class ObjectWithDeprecatedFieldsXmlReflection {
    const ObjectWithDeprecatedFieldsXmlReflection();
}

