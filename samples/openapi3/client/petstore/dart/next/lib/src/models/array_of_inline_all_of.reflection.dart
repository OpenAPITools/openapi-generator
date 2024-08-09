// Model reflection

part of 'array_of_inline_all_of.dart';


//class reflection

class ArrayOfInlineAllOfReflection extends ClassReflection<ArrayOfInlineAllOf> {
  static ArrayOfInlineAllOfReflection instanceGetter() => instance;
  static const instance = ArrayOfInlineAllOfReflection._(
    modelName: r'ArrayOfInlineAllOf',
    className: r'ArrayOfInlineAllOf',
    idPart: PropertyReflection<ArrayOfInlineAllOf, UndefinedWrapper<
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
    namePart: PropertyReflection<ArrayOfInlineAllOf, 
            String

>(
      dartName: r'name',
      nullable: false,
      required: true,
      oasName: r'name',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _nameGetter,
      setter: _nameSetter,
    ),
    arrayAllofDogPropertyPart: PropertyReflection<ArrayOfInlineAllOf, UndefinedWrapper<
    List<
        
            ArrayOfInlineAllOfArrayAllofDogPropertyInner

>

>>(
      dartName: r'arrayAllofDogProperty',
      nullable: false,
      required: false,
      oasName: r'array_allof_dog_property',
      oasType: r'array',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      itemsReflection: ItemsReflection<ArrayOfInlineAllOf, 
            ArrayOfInlineAllOfArrayAllofDogPropertyInner

>(parentReflectionGetter: instanceGetter,classReflection: ArrayOfInlineAllOfArrayAllofDogPropertyInnerReflection.instance,),
      getter: _arrayAllofDogPropertyGetter,
      setter: _arrayAllofDogPropertySetter,
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<ArrayOfInlineAllOf, Object

?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const ArrayOfInlineAllOfReflection._({
    required this.modelName,
    required this.className,
    required this.idPart,
    required this.namePart,
    required this.arrayAllofDogPropertyPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<ArrayOfInlineAllOf, UndefinedWrapper<
            int

>> idPart;
  static UndefinedWrapper<
            int

> _idGetter(ArrayOfInlineAllOf parent) {
    return parent.id;
  }
  static void _idSetter(ArrayOfInlineAllOf parent, UndefinedWrapper<
            int

> value) {
    parent.id = value;
  }
  final PropertyReflection<ArrayOfInlineAllOf, 
            String

> namePart;
  static 
            String

 _nameGetter(ArrayOfInlineAllOf parent) {
    return parent.name;
  }
  static void _nameSetter(ArrayOfInlineAllOf parent, 
            String

 value) {
    parent.name = value;
  }
  final PropertyReflection<ArrayOfInlineAllOf, UndefinedWrapper<
    List<
        
            ArrayOfInlineAllOfArrayAllofDogPropertyInner

>

>> arrayAllofDogPropertyPart;
  static UndefinedWrapper<
    List<
        
            ArrayOfInlineAllOfArrayAllofDogPropertyInner

>

> _arrayAllofDogPropertyGetter(ArrayOfInlineAllOf parent) {
    return parent.arrayAllofDogProperty;
  }
  static void _arrayAllofDogPropertySetter(ArrayOfInlineAllOf parent, UndefinedWrapper<
    List<
        
            ArrayOfInlineAllOfArrayAllofDogPropertyInner

>

> value) {
    parent.arrayAllofDogProperty = value;
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
  List<PropertyReflection<ArrayOfInlineAllOf, dynamic>> get properties => [
    idPart,
namePart,
arrayAllofDogPropertyPart,
  ];

  final AdditionalPropertiesReflection<ArrayOfInlineAllOf, Object

?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<ArrayOfInlineAllOf, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<ArrayOfInlineAllOf, dynamic>> get allOfs => [
    
  ];


  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => ArrayOfInlineAllOf.canDeserialize(src);
  @override
  ArrayOfInlineAllOf Function(Object? src) get deserializeFunction =>
      (src) => ArrayOfInlineAllOf.deserialize(src);

  @override
  Object? Function(ArrayOfInlineAllOf src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of ArrayOfInlineAllOf.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  ArrayOfInlineAllOf example({AggregatedDiscriminatorsResult? discriminators, Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>>
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

    final exampleResult = ArrayOfInlineAllOf(
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
        return result;
      } (),
      arrayAllofDogProperty: () {
        var result = 


    exampleList(() { return 


            
            


    ArrayOfInlineAllOfArrayAllofDogPropertyInnerReflection.instance.example()
    


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


class ArrayOfInlineAllOfXmlReflection {
    const ArrayOfInlineAllOfXmlReflection();
}

