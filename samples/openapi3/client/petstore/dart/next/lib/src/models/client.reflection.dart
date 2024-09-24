// Model reflection

part of 'client.dart';


//class reflection

class ClientReflection extends ModelReflection<Client> {
  static ClientReflection instanceGetter() => instance;
  static const instance = ClientReflection._(
    modelName: r'Client',
    className: r'Client',
    xml: XmlReflection(
),
    clientPart: PropertyReflection<Client, UndefinedWrapper<
            String
>>(
      dartName: r'client',
      nullable: false,
      required: false,
      oasName: r'client',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_clientGetter),
      setter: FunctionWrapper2(_clientSetter),
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
  const ClientReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.clientPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<Client, UndefinedWrapper<
            String
>> clientPart;
  static UndefinedWrapper<
            String
> _clientGetter(Client parent) {
    return parent.client;
  }
  static void _clientSetter(Client parent, UndefinedWrapper<
            String
> value) {
    parent.client = value;
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
  List<PropertyReflection<Client, dynamic>> get properties => [
    clientPart,
  ];

  @override
  final AdditionalPropertiesPart<Client, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(Client instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(Client instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<Client, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  Client empty() {
    return Client(
    );
  }

  @override
  bool operator ==(Object other) {
    return other is ClientReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


