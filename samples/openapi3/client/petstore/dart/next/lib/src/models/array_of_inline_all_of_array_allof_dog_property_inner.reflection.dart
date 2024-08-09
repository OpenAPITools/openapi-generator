// Model reflection

part of 'array_of_inline_all_of_array_allof_dog_property_inner.dart';


//class reflection

class ArrayOfInlineAllOfArrayAllofDogPropertyInnerReflection extends ClassReflection<ArrayOfInlineAllOfArrayAllofDogPropertyInner> {
  static ArrayOfInlineAllOfArrayAllofDogPropertyInnerReflection instanceGetter() => instance;
  static const instance = ArrayOfInlineAllOfArrayAllofDogPropertyInnerReflection._(
    modelName: r'ArrayOfInlineAllOf_array_allof_dog_property_inner',
    className: r'ArrayOfInlineAllOfArrayAllofDogPropertyInner',
    breedPart: PropertyReflection<ArrayOfInlineAllOfArrayAllofDogPropertyInner, UndefinedWrapper<
            String

>>(
      dartName: r'breed',
      nullable: false,
      required: false,
      oasName: r'breed',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _breedGetter,
      setter: _breedSetter,
    ),
    colorPart: PropertyReflection<ArrayOfInlineAllOfArrayAllofDogPropertyInner, UndefinedWrapper<
            String

>>(
      dartName: r'color',
      nullable: false,
      required: false,
      oasName: r'color',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _colorGetter,
      setter: _colorSetter,
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<ArrayOfInlineAllOfArrayAllofDogPropertyInner, Object

?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const ArrayOfInlineAllOfArrayAllofDogPropertyInnerReflection._({
    required this.modelName,
    required this.className,
    required this.breedPart,
    required this.colorPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<ArrayOfInlineAllOfArrayAllofDogPropertyInner, UndefinedWrapper<
            String

>> breedPart;
  static UndefinedWrapper<
            String

> _breedGetter(ArrayOfInlineAllOfArrayAllofDogPropertyInner parent) {
    return parent.breed;
  }
  static void _breedSetter(ArrayOfInlineAllOfArrayAllofDogPropertyInner parent, UndefinedWrapper<
            String

> value) {
    parent.breed = value;
  }
  final PropertyReflection<ArrayOfInlineAllOfArrayAllofDogPropertyInner, UndefinedWrapper<
            String

>> colorPart;
  static UndefinedWrapper<
            String

> _colorGetter(ArrayOfInlineAllOfArrayAllofDogPropertyInner parent) {
    return parent.color;
  }
  static void _colorSetter(ArrayOfInlineAllOfArrayAllofDogPropertyInner parent, UndefinedWrapper<
            String

> value) {
    parent.color = value;
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
  List<PropertyReflection<ArrayOfInlineAllOfArrayAllofDogPropertyInner, dynamic>> get properties => [
    breedPart,
colorPart,
  ];

  final AdditionalPropertiesReflection<ArrayOfInlineAllOfArrayAllofDogPropertyInner, Object

?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<ArrayOfInlineAllOfArrayAllofDogPropertyInner, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<ArrayOfInlineAllOfArrayAllofDogPropertyInner, dynamic>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<ArrayOfInlineAllOfArrayAllofDogPropertyInner, dynamic>> get oneOfs => [
    
  ];
  @override
  List<AnyOfReflection<ArrayOfInlineAllOfArrayAllofDogPropertyInner, dynamic>> get anyOfs => [
    
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => ArrayOfInlineAllOfArrayAllofDogPropertyInner.canDeserialize(src);
  @override
  ArrayOfInlineAllOfArrayAllofDogPropertyInner Function(Object? src) get deserializeFunction =>
      (src) => ArrayOfInlineAllOfArrayAllofDogPropertyInner.deserialize(src);

  @override
  Object? Function(ArrayOfInlineAllOfArrayAllofDogPropertyInner src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of ArrayOfInlineAllOfArrayAllofDogPropertyInner.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  ArrayOfInlineAllOfArrayAllofDogPropertyInner example({AggregatedDiscriminatorsResult? discriminators, Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>>
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

    final exampleResult = ArrayOfInlineAllOfArrayAllofDogPropertyInner(
      breed: () {
        var result = 


            
            


    
    exampleString()


;
        final preSelectedResult = discriminatorExampleResults[breedPart.oasName]?.key.key;
        if (preSelectedResult != null) {
          result = preSelectedResult;
        }
        return UndefinedWrapper(result);
      } (),
      color: () {
        var result = 


            
            


    
    exampleString()


;
        final preSelectedResult = discriminatorExampleResults[colorPart.oasName]?.key.key;
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


class ArrayOfInlineAllOfArrayAllofDogPropertyInnerXmlReflection {
    const ArrayOfInlineAllOfArrayAllofDogPropertyInnerXmlReflection();
}

