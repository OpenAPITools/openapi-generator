// Model reflection

part of 'scalar.dart';


//class reflection

class ScalarReflection extends ClassReflection<Scalar> {
  static ScalarReflection instanceGetter() => instance;
  static const instance = ScalarReflection._(
    modelName: r'Scalar',
    className: r'Scalar',
    
    
    oneOf0Part: OneOfReflection(
      parentReflectionGetter: instanceGetter,
          ),
    
    oneOf1Part: OneOfReflection(
      parentReflectionGetter: instanceGetter,
          ),
    
    oneOf2Part: OneOfReflection(
      parentReflectionGetter: instanceGetter,
          ),
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<Scalar, Object
?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const ScalarReflection._({
    required this.modelName,
    required this.className,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.oneOf0Part,
    
    required this.oneOf1Part,
    
    required this.oneOf2Part,
    
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
  List<PropertyReflection<Scalar, dynamic>> get properties => [
      ];

  final AdditionalPropertiesReflection<Scalar, Object
?> additionalPropertiesPart;

  
  
  final OneOfReflection<Scalar, 
            String
> oneOf0Part;
  
  final OneOfReflection<Scalar, 
            num
> oneOf1Part;
  
  final OneOfReflection<Scalar, 
            bool
> oneOf2Part;
  
  @override
  List<PartReflection<Scalar, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<Scalar, dynamic>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<Scalar, dynamic>> get oneOfs => [
    oneOf0Part,oneOf1Part,oneOf2Part,
  ];
  @override
  List<AnyOfReflection<Scalar, dynamic>> get anyOfs => [
    
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => Scalar.canDeserialize(src);
  @override
  Scalar Function(Object? src) get deserializeFunction =>
      (src) => Scalar.deserialize(src);

  @override
  Object? Function(Scalar src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of Scalar.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  Scalar example({Map<String, (ClassReflection, PropertyReflection)> discriminators = const {}}) {
    final _reflection = this;
    if (discriminators.isEmpty) discriminators = _reflection.aggregatedDiscriminators;
    return Scalar(
      additionalProperties: () { PartReflection? _partReflection = _reflection.additionalPropertiesPart; return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
      
      oneOf0: () {
        PartReflection? _partReflection = _reflection.oneOf0Part;
        return UndefinedWrapper(


            
            


    
    exampleString()


);
      }(),
      
    );
  }
}

class ScalarXmlReflection {
    const ScalarXmlReflection();
}

