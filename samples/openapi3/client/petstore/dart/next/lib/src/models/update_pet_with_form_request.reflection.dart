// Model reflection

part of 'update_pet_with_form_request.dart';


//class reflection

class UpdatePetWithFormRequestReflection extends ModelReflection<UpdatePetWithFormRequest> {
  static UpdatePetWithFormRequestReflection instanceGetter() => instance;
  static const instance = UpdatePetWithFormRequestReflection._(
    modelName: r'updatePetWithForm_request',
    className: r'UpdatePetWithFormRequest',
    xml: XmlReflection(
),
    namePart: PropertyReflection<UpdatePetWithFormRequest, UndefinedWrapper<
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
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_nameGetter),
      setter: FunctionWrapper2(_nameSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
),
    ),
    statusPart: PropertyReflection<UpdatePetWithFormRequest, UndefinedWrapper<
            String
>>(
      dartName: r'status',
      nullable: false,
      required: false,
      oasName: r'status',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_statusGetter),
      setter: FunctionWrapper2(_statusSetter),
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
  const UpdatePetWithFormRequestReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.namePart,
    required this.statusPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<UpdatePetWithFormRequest, UndefinedWrapper<
            String
>> namePart;
  static UndefinedWrapper<
            String
> _nameGetter(UpdatePetWithFormRequest parent) {
    return parent.name;
  }
  static void _nameSetter(UpdatePetWithFormRequest parent, UndefinedWrapper<
            String
> value) {
    parent.name = value;
  }

  final PropertyReflection<UpdatePetWithFormRequest, UndefinedWrapper<
            String
>> statusPart;
  static UndefinedWrapper<
            String
> _statusGetter(UpdatePetWithFormRequest parent) {
    return parent.status;
  }
  static void _statusSetter(UpdatePetWithFormRequest parent, UndefinedWrapper<
            String
> value) {
    parent.status = value;
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
  List<PropertyReflection<UpdatePetWithFormRequest, dynamic>> get properties => [
    namePart,
statusPart,
  ];

  @override
  final AdditionalPropertiesPart<UpdatePetWithFormRequest, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(UpdatePetWithFormRequest instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(UpdatePetWithFormRequest instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<UpdatePetWithFormRequest, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  UpdatePetWithFormRequest empty() {
    return UpdatePetWithFormRequest(
    );
  }

  @override
  bool operator ==(Object other) {
    return other is UpdatePetWithFormRequestReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


