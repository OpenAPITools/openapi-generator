// Model reflection

part of 'free_form_object_test_class.dart';


//class reflection

class FreeFormObjectTestClassReflection extends ClassReflection<FreeFormObjectTestClass> {
  static FreeFormObjectTestClassReflection instanceGetter() => instance;
  static const instance = FreeFormObjectTestClassReflection._(
    modelName: r'FreeFormObjectTestClass',
    className: r'FreeFormObjectTestClass',
    namePart: PropertyReflection<FreeFormObjectTestClass, UndefinedWrapper<
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
    propertiesPart: PropertyReflection<FreeFormObjectTestClass, UndefinedWrapper<
            FreeFormObjectTestClassProperties
>>(
      dartName: r'properties',
      nullable: false,
      required: false,
      oasName: r'properties',
      oasType: r'FreeFormObjectTestClassProperties',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      classReflection: FreeFormObjectTestClassPropertiesReflection.instance,
      getter: _propertiesGetter,
      setter: _propertiesSetter,
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<FreeFormObjectTestClass, Object
?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const FreeFormObjectTestClassReflection._({
    required this.modelName,
    required this.className,
    required this.namePart,
    required this.propertiesPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<FreeFormObjectTestClass, UndefinedWrapper<
            String
>> namePart;
  static UndefinedWrapper<
            String
> _nameGetter(FreeFormObjectTestClass parent) {
    return parent.name;
  }
  static void _nameSetter(FreeFormObjectTestClass parent, UndefinedWrapper<
            String
> value) {
    parent.name = value;
  }
  final PropertyReflection<FreeFormObjectTestClass, UndefinedWrapper<
            FreeFormObjectTestClassProperties
>> propertiesPart;
  static UndefinedWrapper<
            FreeFormObjectTestClassProperties
> _propertiesGetter(FreeFormObjectTestClass parent) {
    return parent.properties;
  }
  static void _propertiesSetter(FreeFormObjectTestClass parent, UndefinedWrapper<
            FreeFormObjectTestClassProperties
> value) {
    parent.properties = value;
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
  List<PropertyReflection<FreeFormObjectTestClass, dynamic>> get properties => [
    namePart,
propertiesPart,
  ];

  final AdditionalPropertiesReflection<FreeFormObjectTestClass, Object
?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<FreeFormObjectTestClass, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<FreeFormObjectTestClass, dynamic>> get allOfs => [
    
  ];


  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => FreeFormObjectTestClass.canDeserialize(src);
  @override
  FreeFormObjectTestClass Function(Object? src) get deserializeFunction =>
      (src) => FreeFormObjectTestClass.deserialize(src);

  @override
  Object? Function(FreeFormObjectTestClass src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of FreeFormObjectTestClass.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  FreeFormObjectTestClass example({Map<String, (ClassReflection, PropertyReflection)> discriminators = const {}}) {
    final _reflection = this;
    if (discriminators.isEmpty) discriminators = _reflection.aggregatedDiscriminators;
    return FreeFormObjectTestClass(
      name: () {
        PartReflection? _partReflection = _reflection.namePart;
        
        final disc = discriminators[r'name'];
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
      properties: () {
        PartReflection? _partReflection = _reflection.propertiesPart;
        
        return UndefinedWrapper(


            
            


    FreeFormObjectTestClassProperties.$reflection.example()
    


);
      }(),
      additionalProperties: () { PartReflection? _partReflection = _reflection.additionalPropertiesPart; return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
      
    );
  }
}

class FreeFormObjectTestClassXmlReflection {
    const FreeFormObjectTestClassXmlReflection();
}

