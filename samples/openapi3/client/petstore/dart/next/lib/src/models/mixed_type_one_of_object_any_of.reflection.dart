// Model reflection

part of 'mixed_type_one_of_object_any_of.dart';


//class reflection

class MixedTypeOneOfObjectAnyOfReflection extends ModelReflection<MixedTypeOneOfObjectAnyOf> {
  static MixedTypeOneOfObjectAnyOfReflection instanceGetter() => instance;
  static const instance = MixedTypeOneOfObjectAnyOfReflection._(
    modelName: r'MixedTypeOneOfObject_anyOf',
    className: r'MixedTypeOneOfObjectAnyOf',
    xml: XmlReflection(
),
    aPart: PropertyReflection<MixedTypeOneOfObjectAnyOf, 
            int
>(
      dartName: r'a',
      nullable: false,
      required: true,
      oasName: r'a',
      oasType: r'integer',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_aGetter),
      setter: FunctionWrapper2(_aSetter),
      reflection: XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forint
        
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
  const MixedTypeOneOfObjectAnyOfReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.aPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<MixedTypeOneOfObjectAnyOf, 
            int
> aPart;
  static 
            int
 _aGetter(MixedTypeOneOfObjectAnyOf parent) {
    return parent.a;
  }
  static void _aSetter(MixedTypeOneOfObjectAnyOf parent, 
            int
 value) {
    parent.a = value;
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
  List<PropertyReflection<MixedTypeOneOfObjectAnyOf, dynamic>> get properties => [
    aPart,
  ];

  @override
  final AdditionalPropertiesPart<MixedTypeOneOfObjectAnyOf, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(MixedTypeOneOfObjectAnyOf instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(MixedTypeOneOfObjectAnyOf instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<MixedTypeOneOfObjectAnyOf, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  MixedTypeOneOfObjectAnyOf empty() {
    return MixedTypeOneOfObjectAnyOf(
      a: aPart.reflection.emptyFunction(),
    );
  }

  @override
  bool operator ==(Object other) {
    return other is MixedTypeOneOfObjectAnyOfReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


