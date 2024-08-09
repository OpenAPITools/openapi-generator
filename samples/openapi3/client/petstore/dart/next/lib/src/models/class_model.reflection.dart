// Model reflection

part of 'class_model.dart';


//class reflection

class ClassModelReflection extends ClassReflection<ClassModel> {
  static ClassModelReflection instanceGetter() => instance;
  static const instance = ClassModelReflection._(
    modelName: r'ClassModel',
    className: r'ClassModel',
    propertyClassPart: PropertyReflection<ClassModel, UndefinedWrapper<
            String

>>(
      dartName: r'propertyClass',
      nullable: false,
      required: false,
      oasName: r'_class',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _propertyClassGetter,
      setter: _propertyClassSetter,
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<ClassModel, Object

?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const ClassModelReflection._({
    required this.modelName,
    required this.className,
    required this.propertyClassPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<ClassModel, UndefinedWrapper<
            String

>> propertyClassPart;
  static UndefinedWrapper<
            String

> _propertyClassGetter(ClassModel parent) {
    return parent.propertyClass;
  }
  static void _propertyClassSetter(ClassModel parent, UndefinedWrapper<
            String

> value) {
    parent.propertyClass = value;
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
  List<PropertyReflection<ClassModel, dynamic>> get properties => [
    propertyClassPart,
  ];

  final AdditionalPropertiesReflection<ClassModel, Object

?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<ClassModel, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<ClassModel, dynamic>> get allOfs => [
    
  ];


  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => ClassModel.canDeserialize(src);
  @override
  ClassModel Function(Object? src) get deserializeFunction =>
      (src) => ClassModel.deserialize(src);

  @override
  Object? Function(ClassModel src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of ClassModel.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  ClassModel example({AggregatedDiscriminatorsResult? discriminators, Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>>
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

    final exampleResult = ClassModel(
      propertyClass: () {
        var result = 


            
            


    
    exampleString()


;
        final preSelectedResult = discriminatorExampleResults[propertyClassPart.oasName]?.key.key;
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


class ClassModelXmlReflection {
    const ClassModelXmlReflection();
}

