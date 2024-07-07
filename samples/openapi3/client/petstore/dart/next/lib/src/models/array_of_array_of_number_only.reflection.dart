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
  ArrayOfArrayOfNumberOnly example({Map<String, (ClassReflection, PropertyReflection)> discriminators = const {}}) {
    final _reflection = this;
    if (discriminators.isEmpty) discriminators = _reflection.aggregatedDiscriminators;
    return ArrayOfArrayOfNumberOnly(
      arrayArrayNumber: () {
        PartReflection? _partReflection = _reflection.arrayArrayNumberPart;
        
        return UndefinedWrapper(


    exampleList(() { _partReflection = _partReflection?.itemsReflection; return 


    exampleList(() { _partReflection = _partReflection?.itemsReflection; return 


            
            


    
    examplenum()


; })



; })



);
      }(),
      additionalProperties: () { PartReflection? _partReflection = _reflection.additionalPropertiesPart; return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
      
    );
  }
}

class ArrayOfArrayOfNumberOnlyXmlReflection {
    const ArrayOfArrayOfNumberOnlyXmlReflection();
}

