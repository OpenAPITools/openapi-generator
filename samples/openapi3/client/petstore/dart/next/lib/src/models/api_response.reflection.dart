// Model reflection

part of 'api_response.dart';


//class reflection

class ApiResponseReflection extends ModelReflection<ApiResponse> {
  static ApiResponseReflection instanceGetter() => instance;
  static const instance = ApiResponseReflection._(
    modelName: r'ApiResponse',
    className: r'ApiResponse',
    xml: XmlReflection(
),
    codePart: PropertyReflection<ApiResponse, UndefinedWrapper<
            int
>>(
      dartName: r'code',
      nullable: false,
      required: false,
      oasName: r'code',
      oasType: r'integer',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_codeGetter),
      setter: FunctionWrapper2(_codeSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forint
        
,
)
),
    ),
    typePart: PropertyReflection<ApiResponse, UndefinedWrapper<
            String
>>(
      dartName: r'type',
      nullable: false,
      required: false,
      oasName: r'type',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_typeGetter),
      setter: FunctionWrapper2(_typeSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
),
    ),
    messagePart: PropertyReflection<ApiResponse, UndefinedWrapper<
            String
>>(
      dartName: r'message',
      nullable: false,
      required: false,
      oasName: r'message',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_messageGetter),
      setter: FunctionWrapper2(_messageSetter),
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
  const ApiResponseReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.codePart,
    required this.typePart,
    required this.messagePart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<ApiResponse, UndefinedWrapper<
            int
>> codePart;
  static UndefinedWrapper<
            int
> _codeGetter(ApiResponse parent) {
    return parent.code;
  }
  static void _codeSetter(ApiResponse parent, UndefinedWrapper<
            int
> value) {
    parent.code = value;
  }

  final PropertyReflection<ApiResponse, UndefinedWrapper<
            String
>> typePart;
  static UndefinedWrapper<
            String
> _typeGetter(ApiResponse parent) {
    return parent.type;
  }
  static void _typeSetter(ApiResponse parent, UndefinedWrapper<
            String
> value) {
    parent.type = value;
  }

  final PropertyReflection<ApiResponse, UndefinedWrapper<
            String
>> messagePart;
  static UndefinedWrapper<
            String
> _messageGetter(ApiResponse parent) {
    return parent.message;
  }
  static void _messageSetter(ApiResponse parent, UndefinedWrapper<
            String
> value) {
    parent.message = value;
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
  List<PropertyReflection<ApiResponse, dynamic>> get properties => [
    codePart,
typePart,
messagePart,
  ];

  @override
  final AdditionalPropertiesPart<ApiResponse, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(ApiResponse instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(ApiResponse instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<ApiResponse, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  ApiResponse empty() {
    return ApiResponse(
    );
  }

  @override
  bool operator ==(Object other) {
    return other is ApiResponseReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


