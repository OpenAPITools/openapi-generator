// Model reflection

part of 'array_of_array_of_number_only.dart';


//class reflection

class ArrayOfArrayOfNumberOnlyReflection extends ClassReflection<ArrayOfArrayOfNumberOnly> {
  static ArrayOfArrayOfNumberOnlyReflection instanceGetter() => instance;
  static const instance = ArrayOfArrayOfNumberOnlyReflection._(
    modelName: r'ArrayOfArrayOfNumberOnly',
    className: r'ArrayOfArrayOfNumberOnly',
    arrayArrayNumberPart: PropertyReflection<ArrayOfArrayOfNumberOnly, UndefinedWrapper<
    List<
        
    List<
        
            num

>

>

>>(
      dartName: r'arrayArrayNumber',
      nullable: false,
      required: false,
      oasName: r'ArrayArrayNumber',
      oasType: r'array',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      itemsReflection: ItemsReflection<ArrayOfArrayOfNumberOnly, 
    List<
        
            num

>

>(parentReflectionGetter: instanceGetter,itemsReflection: ItemsReflection<ArrayOfArrayOfNumberOnly, 
            num

>(parentReflectionGetter: instanceGetter,)),
      getter: _arrayArrayNumberGetter,
      setter: _arrayArrayNumberSetter,
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<ArrayOfArrayOfNumberOnly, Object

?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const ArrayOfArrayOfNumberOnlyReflection._({
    required this.modelName,
    required this.className,
    required this.arrayArrayNumberPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<ArrayOfArrayOfNumberOnly, UndefinedWrapper<
    List<
        
    List<
        
            num

>

>

>> arrayArrayNumberPart;
  static UndefinedWrapper<
    List<
        
    List<
        
            num

>

>

> _arrayArrayNumberGetter(ArrayOfArrayOfNumberOnly parent) {
    return parent.arrayArrayNumber;
  }
  static void _arrayArrayNumberSetter(ArrayOfArrayOfNumberOnly parent, UndefinedWrapper<
    List<
        
    List<
        
            num

>

>

> value) {
    parent.arrayArrayNumber = value;
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
  List<PropertyReflection<ArrayOfArrayOfNumberOnly, dynamic>> get properties => [
    arrayArrayNumberPart,
  ];

  final AdditionalPropertiesReflection<ArrayOfArrayOfNumberOnly, Object

?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<ArrayOfArrayOfNumberOnly, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<ArrayOfArrayOfNumberOnly, dynamic>> get allOfs => [
    
  ];


  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => ArrayOfArrayOfNumberOnly.canDeserialize(src);
  @override
  ArrayOfArrayOfNumberOnly Function(Object? src) get deserializeFunction =>
      (src) => ArrayOfArrayOfNumberOnly.deserialize(src);

  @override
  Object? Function(ArrayOfArrayOfNumberOnly src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of ArrayOfArrayOfNumberOnly.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  ArrayOfArrayOfNumberOnly example({AggregatedDiscriminatorsResult? discriminators, Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>>
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

    final exampleResult = ArrayOfArrayOfNumberOnly(
      arrayArrayNumber: () {
        var result = 


    exampleList(() { return 


    exampleList(() { return 


            
            


    
    examplenum()


; })



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


class ArrayOfArrayOfNumberOnlyXmlReflection {
    const ArrayOfArrayOfNumberOnlyXmlReflection();
}

