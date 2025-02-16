// Model reflection

part of 'class_model.dart';


//class reflection

class ClassModelReflection extends ModelReflection<ClassModel> {
  static ClassModelReflection instanceGetter() => instance;
  static const instance = ClassModelReflection._(
    modelName: r'ClassModel',
    className: r'ClassModel',
    xml: XmlReflection(
),
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
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_propertyClassGetter),
      setter: FunctionWrapper2(_propertyClassSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
),
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesPart(
      parentReflectionGetter: instanceGetter,
      itemReflection: XmlReflectionWrapper(
    xml: XmlReflection(
),
    NullableReflection(ObjectReflection()
),
)
,
      getter: FunctionWrapper1(_AdditionalPropertiesGetter),
      setter: FunctionWrapper2(_AdditionalPropertiesSetter),
    ),
  );
  const ClassModelReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
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
  final Map<String, ModelReflection> discriminatorMappings;
  @override
  final Map<String, ModelReflection> discriminatorImplicitMappings;
  @override
  final String? discriminatorKey;
  @override
  final String modelName;
  @override
  final String className;
  @override
  final XmlReflection xml;

  @override
  List<PropertyReflection<ClassModel, dynamic>> get properties => [
    propertyClassPart,
  ];

  @override
  final AdditionalPropertiesPart<ClassModel, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(ClassModel instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(ClassModel instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<ClassModel, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  ClassModel empty() {
    return ClassModel(
    );
  }

  @override
  bool operator ==(Object other) {
    return other is ClassModelReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


