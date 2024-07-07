// Model reflection

part of 'free_form_object_test_class_properties.dart';


//class reflection

class FreeFormObjectTestClassPropertiesReflection extends ClassReflection<FreeFormObjectTestClassProperties> {
  static FreeFormObjectTestClassPropertiesReflection instanceGetter() => instance;
  static const instance = FreeFormObjectTestClassPropertiesReflection._(
    modelName: r'FreeFormObjectTestClass_properties',
    className: r'FreeFormObjectTestClassProperties',
    
    
    oneOf0Part: OneOfReflection(
      parentReflectionGetter: instanceGetter,
          ),
    
    oneOf1Part: OneOfReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<FreeFormObjectTestClassProperties, Object
?>(parentReflectionGetter: instanceGetter,),
          ),
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<FreeFormObjectTestClassProperties, Object
?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const FreeFormObjectTestClassPropertiesReflection._({
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
  List<PropertyReflection<FreeFormObjectTestClassProperties, dynamic>> get properties => [
      ];

  final AdditionalPropertiesReflection<FreeFormObjectTestClassProperties, Object
?> additionalPropertiesPart;

  
  
  final OneOfReflection<FreeFormObjectTestClassProperties, 
            String
> oneOf0Part;
  
  final OneOfReflection<FreeFormObjectTestClassProperties, 
    Map<String, 
        Object
?>
> oneOf1Part;
  
  @override
  List<PartReflection<FreeFormObjectTestClassProperties, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<FreeFormObjectTestClassProperties, dynamic>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<FreeFormObjectTestClassProperties, dynamic>> get oneOfs => [
    oneOf0Part,oneOf1Part,
  ];
  @override
  List<AnyOfReflection<FreeFormObjectTestClassProperties, dynamic>> get anyOfs => [
    
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => FreeFormObjectTestClassProperties.canDeserialize(src);
  @override
  FreeFormObjectTestClassProperties Function(Object? src) get deserializeFunction =>
      (src) => FreeFormObjectTestClassProperties.deserialize(src);

  @override
  Object? Function(FreeFormObjectTestClassProperties src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of FreeFormObjectTestClassProperties.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  FreeFormObjectTestClassProperties example({Map<String, (ClassReflection, PropertyReflection)> discriminators = const {}}) {
    final _reflection = this;
    if (discriminators.isEmpty) discriminators = _reflection.aggregatedDiscriminators;
    return FreeFormObjectTestClassProperties(
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

class FreeFormObjectTestClassPropertiesXmlReflection {
    const FreeFormObjectTestClassPropertiesXmlReflection();
}

