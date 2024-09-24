// Model reflection

part of 'all_of_model_array_any_of_all_of_link_list_column1_value.dart';


//class reflection

class AllOfModelArrayAnyOfAllOfLinkListColumn1ValueReflection extends ModelReflection<AllOfModelArrayAnyOfAllOfLinkListColumn1Value> {
  static AllOfModelArrayAnyOfAllOfLinkListColumn1ValueReflection instanceGetter() => instance;
  static const instance = AllOfModelArrayAnyOfAllOfLinkListColumn1ValueReflection._(
    modelName: r'AllOfModelArrayAnyOf_allOf_linkListColumn1_value',
    className: r'AllOfModelArrayAnyOfAllOfLinkListColumn1Value',
    xml: XmlReflection(
),
    
    
    anyOf0Part: AllOfModelArrayAnyOfAllOfLinkListColumn1ValueAnyOf0Part(
      parentReflectionGetter: instanceGetter,
    ),
    
    anyOf1Part: AllOfModelArrayAnyOfAllOfLinkListColumn1ValueAnyOf1Part(
      parentReflectionGetter: instanceGetter,
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
  const AllOfModelArrayAnyOfAllOfLinkListColumn1ValueReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.anyOf0Part,
    
    required this.anyOf1Part,
    
    required this.additionalPropertiesPart,
  });


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
  List<PropertyReflection<AllOfModelArrayAnyOfAllOfLinkListColumn1Value, dynamic>> get properties => [
      ];

  @override
  final AdditionalPropertiesPart<AllOfModelArrayAnyOfAllOfLinkListColumn1Value, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(AllOfModelArrayAnyOfAllOfLinkListColumn1Value instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(AllOfModelArrayAnyOfAllOfLinkListColumn1Value instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  
  final AllOfModelArrayAnyOfAllOfLinkListColumn1ValueAnyOf0Part anyOf0Part;
  
  final AllOfModelArrayAnyOfAllOfLinkListColumn1ValueAnyOf1Part anyOf1Part;
  

  @override
  List<AllOfReflection<AllOfModelArrayAnyOfAllOfLinkListColumn1Value, Object>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<AllOfModelArrayAnyOfAllOfLinkListColumn1Value, Object>> get oneOfs => [
    
  ];
  @override
  List<AnyOfReflection<AllOfModelArrayAnyOfAllOfLinkListColumn1Value, Object>> get anyOfs => [
    anyOf0Part,anyOf1Part,
  ];


  /// Creates an empty instance used as a starting point for deserialization.
  @override
  AllOfModelArrayAnyOfAllOfLinkListColumn1Value empty() {
    return AllOfModelArrayAnyOfAllOfLinkListColumn1Value(
    );
  }

  @override
  bool operator ==(Object other) {
    return other is AllOfModelArrayAnyOfAllOfLinkListColumn1ValueReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}

class AllOfModelArrayAnyOfAllOfLinkListColumn1ValueAnyOf0Part extends AnyOfReflection<AllOfModelArrayAnyOfAllOfLinkListColumn1Value, 
            User
> {

  const AllOfModelArrayAnyOfAllOfLinkListColumn1ValueAnyOf0Part({
  required AllOfModelArrayAnyOfAllOfLinkListColumn1ValueReflection Function() super.parentReflectionGetter,
});
@override
FunctionWrapper1<UndefinedWrapper<
            User
>, AllOfModelArrayAnyOfAllOfLinkListColumn1Value> get getter => FunctionWrapper1(_getter);
@override
FunctionWrapper2<void, AllOfModelArrayAnyOfAllOfLinkListColumn1Value, UndefinedWrapper<
            User
>> get setter => FunctionWrapper2(_setter);

static UndefinedWrapper<
            User
> _getter(AllOfModelArrayAnyOfAllOfLinkListColumn1Value src) {
  return src.anyOf0;
}
static void _setter(AllOfModelArrayAnyOfAllOfLinkListColumn1Value src, UndefinedWrapper<
            User
> value) {
  src.anyOf0 = value;
}

@override
UndefinedWrapperReflection<
            User
> get reflection => UndefinedWrapperReflection(
  XmlReflectionWrapper(
    xml: XmlReflection(
    xmlName: r'User',
),
    
            
        
        
            
                User.$reflection
        

  ),
);

  UndefinedWrapper<
            User
> example({
    required AggregatedDiscriminatorsResult discriminators,
    required Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ModelReflection>> discriminatorExampleResults,
    required Map<String, Uint8List>? fileCache,
  }) {
    if (discriminatorExampleResults.isNotEmpty) {
      if (!discriminatorExampleResults.values
          .any((e) => e.value == reflection.subReflection)) {
        return UndefinedWrapper.undefined();
      }
    }
    return reflection.example();
  }
}
class AllOfModelArrayAnyOfAllOfLinkListColumn1ValueAnyOf1Part extends AnyOfReflection<AllOfModelArrayAnyOfAllOfLinkListColumn1Value, 
            Tag
> {

  const AllOfModelArrayAnyOfAllOfLinkListColumn1ValueAnyOf1Part({
  required AllOfModelArrayAnyOfAllOfLinkListColumn1ValueReflection Function() super.parentReflectionGetter,
});
@override
FunctionWrapper1<UndefinedWrapper<
            Tag
>, AllOfModelArrayAnyOfAllOfLinkListColumn1Value> get getter => FunctionWrapper1(_getter);
@override
FunctionWrapper2<void, AllOfModelArrayAnyOfAllOfLinkListColumn1Value, UndefinedWrapper<
            Tag
>> get setter => FunctionWrapper2(_setter);

static UndefinedWrapper<
            Tag
> _getter(AllOfModelArrayAnyOfAllOfLinkListColumn1Value src) {
  return src.anyOf1;
}
static void _setter(AllOfModelArrayAnyOfAllOfLinkListColumn1Value src, UndefinedWrapper<
            Tag
> value) {
  src.anyOf1 = value;
}

@override
UndefinedWrapperReflection<
            Tag
> get reflection => UndefinedWrapperReflection(
  XmlReflectionWrapper(
    xml: XmlReflection(
    xmlName: r'Tag',
),
    
            
        
        
            
                Tag.$reflection
        

  ),
);

  UndefinedWrapper<
            Tag
> example({
    required AggregatedDiscriminatorsResult discriminators,
    required Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ModelReflection>> discriminatorExampleResults,
    required Map<String, Uint8List>? fileCache,
  }) {
    if (discriminatorExampleResults.isNotEmpty) {
      if (!discriminatorExampleResults.values
          .any((e) => e.value == reflection.subReflection)) {
        return UndefinedWrapper.undefined();
      }
    }
    return reflection.example();
  }
}

