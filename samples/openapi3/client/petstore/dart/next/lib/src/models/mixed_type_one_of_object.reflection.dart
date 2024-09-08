// Model reflection

part of 'mixed_type_one_of_object.dart';


//class reflection

class MixedTypeOneOfObjectReflection extends ModelReflection<MixedTypeOneOfObject> {
  static MixedTypeOneOfObjectReflection instanceGetter() => instance;
  static const instance = MixedTypeOneOfObjectReflection._(
    modelName: r'MixedTypeOneOfObject',
    className: r'MixedTypeOneOfObject',
    xml: XmlReflection(
),
    cPart: PropertyReflection<MixedTypeOneOfObject, UndefinedWrapper<
            bool
>>(
      dartName: r'c',
      nullable: false,
      required: false,
      oasName: r'c',
      oasType: r'boolean',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_cGetter),
      setter: FunctionWrapper2(_cSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forbool
        
,
)
),
    ),
    
    
    anyOf0Part: MixedTypeOneOfObjectAnyOf0Part(
      parentReflectionGetter: instanceGetter,
    ),
    
    anyOf1Part: MixedTypeOneOfObjectAnyOf1Part(
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
  const MixedTypeOneOfObjectReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.cPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.anyOf0Part,
    
    required this.anyOf1Part,
    
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<MixedTypeOneOfObject, UndefinedWrapper<
            bool
>> cPart;
  static UndefinedWrapper<
            bool
> _cGetter(MixedTypeOneOfObject parent) {
    return parent.c;
  }
  static void _cSetter(MixedTypeOneOfObject parent, UndefinedWrapper<
            bool
> value) {
    parent.c = value;
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
  List<PropertyReflection<MixedTypeOneOfObject, dynamic>> get properties => [
    cPart,
  ];

  @override
  final AdditionalPropertiesPart<MixedTypeOneOfObject, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(MixedTypeOneOfObject instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(MixedTypeOneOfObject instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  
  final MixedTypeOneOfObjectAnyOf0Part anyOf0Part;
  
  final MixedTypeOneOfObjectAnyOf1Part anyOf1Part;
  

  @override
  List<AllOfReflection<MixedTypeOneOfObject, Object>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<MixedTypeOneOfObject, Object>> get oneOfs => [
    
  ];
  @override
  List<AnyOfReflection<MixedTypeOneOfObject, Object>> get anyOfs => [
    anyOf0Part,anyOf1Part,
  ];


  /// Creates an empty instance used as a starting point for deserialization.
  @override
  MixedTypeOneOfObject empty() {
    return MixedTypeOneOfObject(
    );
  }

  @override
  bool operator ==(Object other) {
    return other is MixedTypeOneOfObjectReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}

class MixedTypeOneOfObjectAnyOf0Part extends AnyOfReflection<MixedTypeOneOfObject, 
            MixedTypeOneOfObjectAnyOf
> {

  const MixedTypeOneOfObjectAnyOf0Part({
  required MixedTypeOneOfObjectReflection Function() super.parentReflectionGetter,
});
@override
FunctionWrapper1<UndefinedWrapper<
            MixedTypeOneOfObjectAnyOf
>, MixedTypeOneOfObject> get getter => FunctionWrapper1(_getter);
@override
FunctionWrapper2<void, MixedTypeOneOfObject, UndefinedWrapper<
            MixedTypeOneOfObjectAnyOf
>> get setter => FunctionWrapper2(_setter);

static UndefinedWrapper<
            MixedTypeOneOfObjectAnyOf
> _getter(MixedTypeOneOfObject src) {
  return src.anyOf0;
}
static void _setter(MixedTypeOneOfObject src, UndefinedWrapper<
            MixedTypeOneOfObjectAnyOf
> value) {
  src.anyOf0 = value;
}

@override
UndefinedWrapperReflection<
            MixedTypeOneOfObjectAnyOf
> get reflection => UndefinedWrapperReflection(
  XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                MixedTypeOneOfObjectAnyOf.$reflection
        

  ),
);

  UndefinedWrapper<
            MixedTypeOneOfObjectAnyOf
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
class MixedTypeOneOfObjectAnyOf1Part extends AnyOfReflection<MixedTypeOneOfObject, 
            MixedTypeOneOfObjectAnyOf1
> {

  const MixedTypeOneOfObjectAnyOf1Part({
  required MixedTypeOneOfObjectReflection Function() super.parentReflectionGetter,
});
@override
FunctionWrapper1<UndefinedWrapper<
            MixedTypeOneOfObjectAnyOf1
>, MixedTypeOneOfObject> get getter => FunctionWrapper1(_getter);
@override
FunctionWrapper2<void, MixedTypeOneOfObject, UndefinedWrapper<
            MixedTypeOneOfObjectAnyOf1
>> get setter => FunctionWrapper2(_setter);

static UndefinedWrapper<
            MixedTypeOneOfObjectAnyOf1
> _getter(MixedTypeOneOfObject src) {
  return src.anyOf1;
}
static void _setter(MixedTypeOneOfObject src, UndefinedWrapper<
            MixedTypeOneOfObjectAnyOf1
> value) {
  src.anyOf1 = value;
}

@override
UndefinedWrapperReflection<
            MixedTypeOneOfObjectAnyOf1
> get reflection => UndefinedWrapperReflection(
  XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                MixedTypeOneOfObjectAnyOf1.$reflection
        

  ),
);

  UndefinedWrapper<
            MixedTypeOneOfObjectAnyOf1
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

