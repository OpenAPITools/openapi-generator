// Model reflection

part of 'array_one_of.dart';


//class reflection

class ArrayOneOfReflection extends ClassReflection<ArrayOneOf> {
  static ArrayOneOfReflection instanceGetter() => instance;
  static const instance = ArrayOneOfReflection._(
    modelName: r'ArrayOneOf',
    className: r'ArrayOneOf',
    
    
    oneOf0Part: OneOfReflection(
      parentReflectionGetter: instanceGetter,
          ),
    
    oneOf1Part: OneOfReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<ArrayOneOf, 
            String
>(parentReflectionGetter: instanceGetter,),
          ),
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<ArrayOneOf, Object
?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const ArrayOneOfReflection._({
    required this.modelName,
    required this.className,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.oneOf0Part,
    
    required this.oneOf1Part,
    
    required this.additionalPropertiesPart,
  });




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
  List<PropertyReflection<ArrayOneOf, dynamic>> get properties => [
      ];

  final AdditionalPropertiesReflection<ArrayOneOf, Object
?> additionalPropertiesPart;

  
  
  final OneOfReflection<ArrayOneOf, 
            int
> oneOf0Part;
  
  final OneOfReflection<ArrayOneOf, 
    List<
        
            String
>
> oneOf1Part;
  
  @override
  List<PartReflection<ArrayOneOf, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<ArrayOneOf, dynamic>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<ArrayOneOf, dynamic>> get oneOfs => [
    oneOf0Part,oneOf1Part,
  ];
  @override
  List<AnyOfReflection<ArrayOneOf, dynamic>> get anyOfs => [
    
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => ArrayOneOf.canDeserialize(src);
  @override
  ArrayOneOf Function(Object? src) get deserializeFunction =>
      (src) => ArrayOneOf.deserialize(src);

  @override
  Object? Function(ArrayOneOf src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of ArrayOneOf.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  ArrayOneOf example({Map<String, (ClassReflection, PropertyReflection)> discriminators = const {}}) {
    final _reflection = this;
    if (discriminators.isEmpty) discriminators = _reflection.aggregatedDiscriminators;
    return ArrayOneOf(
      additionalProperties: () { PartReflection? _partReflection = _reflection.additionalPropertiesPart; return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
      
      oneOf0: () {
        PartReflection? _partReflection = _reflection.oneOf0Part;
        return UndefinedWrapper(


            
            


    
    exampleint()


);
      }(),
      
    );
  }
}

class ArrayOneOfXmlReflection {
    const ArrayOneOfXmlReflection();
}

