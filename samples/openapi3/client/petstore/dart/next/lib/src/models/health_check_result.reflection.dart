// Model reflection

part of 'health_check_result.dart';


//class reflection

class HealthCheckResultReflection extends ModelReflection<HealthCheckResult> {
  static HealthCheckResultReflection instanceGetter() => instance;
  static const instance = HealthCheckResultReflection._(
    modelName: r'HealthCheckResult',
    className: r'HealthCheckResult',
    xml: XmlReflection(
),
    nullableMessagePart: PropertyReflection<HealthCheckResult, UndefinedWrapper<
            String
?>>(
      dartName: r'nullableMessage',
      nullable: true,
      required: false,
      oasName: r'NullableMessage',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_nullableMessageGetter),
      setter: FunctionWrapper2(_nullableMessageSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    NullableReflection(
            
        
        
            
                PrimitiveReflection.forString
        
),
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
  const HealthCheckResultReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.nullableMessagePart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<HealthCheckResult, UndefinedWrapper<
            String
?>> nullableMessagePart;
  static UndefinedWrapper<
            String
?> _nullableMessageGetter(HealthCheckResult parent) {
    return parent.nullableMessage;
  }
  static void _nullableMessageSetter(HealthCheckResult parent, UndefinedWrapper<
            String
?> value) {
    parent.nullableMessage = value;
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
  List<PropertyReflection<HealthCheckResult, dynamic>> get properties => [
    nullableMessagePart,
  ];

  @override
  final AdditionalPropertiesPart<HealthCheckResult, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(HealthCheckResult instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(HealthCheckResult instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<HealthCheckResult, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  HealthCheckResult empty() {
    return HealthCheckResult(
    );
  }

  @override
  bool operator ==(Object other) {
    return other is HealthCheckResultReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


