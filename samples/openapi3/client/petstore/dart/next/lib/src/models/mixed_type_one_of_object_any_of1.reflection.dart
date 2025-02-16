// Model reflection

part of 'mixed_type_one_of_object_any_of1.dart';


//class reflection

class MixedTypeOneOfObjectAnyOf1Reflection extends ModelReflection<MixedTypeOneOfObjectAnyOf1> {
  static MixedTypeOneOfObjectAnyOf1Reflection instanceGetter() => instance;
  static const instance = MixedTypeOneOfObjectAnyOf1Reflection._(
    modelName: r'MixedTypeOneOfObject_anyOf_1',
    className: r'MixedTypeOneOfObjectAnyOf1',
    xml: XmlReflection(
),
    bPart: PropertyReflection<MixedTypeOneOfObjectAnyOf1, 
            num
>(
      dartName: r'b',
      nullable: false,
      required: true,
      oasName: r'b',
      oasType: r'number',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_bGetter),
      setter: FunctionWrapper2(_bSetter),
      reflection: XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.fornum
        
,
)
,
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
  const MixedTypeOneOfObjectAnyOf1Reflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.bPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<MixedTypeOneOfObjectAnyOf1, 
            num
> bPart;
  static 
            num
 _bGetter(MixedTypeOneOfObjectAnyOf1 parent) {
    return parent.b;
  }
  static void _bSetter(MixedTypeOneOfObjectAnyOf1 parent, 
            num
 value) {
    parent.b = value;
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
  List<PropertyReflection<MixedTypeOneOfObjectAnyOf1, dynamic>> get properties => [
    bPart,
  ];

  @override
  final AdditionalPropertiesPart<MixedTypeOneOfObjectAnyOf1, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(MixedTypeOneOfObjectAnyOf1 instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(MixedTypeOneOfObjectAnyOf1 instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<MixedTypeOneOfObjectAnyOf1, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  MixedTypeOneOfObjectAnyOf1 empty() {
    return MixedTypeOneOfObjectAnyOf1(
      b: bPart.reflection.emptyFunction(),
    );
  }

  @override
  bool operator ==(Object other) {
    return other is MixedTypeOneOfObjectAnyOf1Reflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


