// Model reflection

part of 'mammal.dart';


//class reflection

class MammalReflection extends ModelReflection<Mammal> {
  static MammalReflection instanceGetter() => instance;
  static const instance = MammalReflection._(
    modelName: r'mammal',
    className: r'Mammal',
    xml: XmlReflection(
),
    classNamePart: PropertyReflection<Mammal, 
            String
>(
      dartName: r'className',
      nullable: false,
      required: true,
      oasName: r'className',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: true,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_classNameGetter),
      setter: FunctionWrapper2(_classNameSetter),
      reflection: XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
,
    ),
    discriminatorKey: r'className',
    discriminatorImplicitMappings: const {
      r'Pig': PigReflection.instance,
      r'Whale': WhaleReflection.instance,
      r'Zebra': ZebraReflection.instance,
    },
    discriminatorMappings: const {
      r'Pig': PigReflection.instance,
      r'whale': WhaleReflection.instance,
      r'zebra': ZebraReflection.instance,
    },
    
    
    oneOf0Part: MammalOneOf0Part(
      parentReflectionGetter: instanceGetter,
    ),
    
    oneOf1Part: MammalOneOf1Part(
      parentReflectionGetter: instanceGetter,
    ),
    
    oneOf2Part: MammalOneOf2Part(
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
  const MammalReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.classNamePart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.oneOf0Part,
    
    required this.oneOf1Part,
    
    required this.oneOf2Part,
    
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<Mammal, 
            String
> classNamePart;
  static 
            String
 _classNameGetter(Mammal parent) {
    return parent.className;
  }
  static void _classNameSetter(Mammal parent, 
            String
 value) {
    parent.className = value;
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
  List<PropertyReflection<Mammal, dynamic>> get properties => [
    classNamePart,
  ];

  @override
  final AdditionalPropertiesPart<Mammal, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(Mammal instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(Mammal instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  
  final MammalOneOf0Part oneOf0Part;
  
  final MammalOneOf1Part oneOf1Part;
  
  final MammalOneOf2Part oneOf2Part;
  

  @override
  List<AllOfReflection<Mammal, Object>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<Mammal, Object>> get oneOfs => [
    oneOf0Part,oneOf1Part,oneOf2Part,
  ];
  @override
  List<AnyOfReflection<Mammal, Object>> get anyOfs => [
    
  ];


  /// Creates an empty instance used as a starting point for deserialization.
  @override
  Mammal empty() {
    return Mammal(
      className: classNamePart.reflection.emptyFunction(),
    );
  }

  @override
  bool operator ==(Object other) {
    return other is MammalReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


class MammalOneOf0Part extends OneOfReflection<Mammal, 
            Whale
> {

  const MammalOneOf0Part({
  required MammalReflection Function() super.parentReflectionGetter,
});
@override
FunctionWrapper1<UndefinedWrapper<
            Whale
>, Mammal> get getter => FunctionWrapper1(_getter);
@override
FunctionWrapper2<void, Mammal, UndefinedWrapper<
            Whale
>> get setter => FunctionWrapper2(_setter);

static UndefinedWrapper<
            Whale
> _getter(Mammal src) {
  return src.oneOf0;
}
static void _setter(Mammal src, UndefinedWrapper<
            Whale
> value) {
  src.oneOf0 = value;
}

@override
UndefinedWrapperReflection<
            Whale
> get reflection => UndefinedWrapperReflection(
  XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                Whale.$reflection
        

  ),
);

  UndefinedWrapper<
            Whale
> example({
    required AggregatedDiscriminatorsResult discriminators,
    required Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ModelReflection>> discriminatorExampleResults,
    required Map<String, Uint8List>? fileCache,
  }) {
    if (discriminatorExampleResults.isEmpty) {
      //return undefined for non-first oneOfs.
      // An example SHOULD be generated
    } else {
      // if this reflection wasn't a result of any property, don't generate an example.

      if (!discriminatorExampleResults.values
          .any((e) => e.value == reflection.subReflection)) {
        // if there are no discriminator examples targetting the current class:
        return UndefinedWrapper.undefined();
      } else {
        // An example SHOULD be generated
      }
    }
    return reflection.example();
  }
}

class MammalOneOf1Part extends OneOfReflection<Mammal, 
            Zebra
> {

  const MammalOneOf1Part({
  required MammalReflection Function() super.parentReflectionGetter,
});
@override
FunctionWrapper1<UndefinedWrapper<
            Zebra
>, Mammal> get getter => FunctionWrapper1(_getter);
@override
FunctionWrapper2<void, Mammal, UndefinedWrapper<
            Zebra
>> get setter => FunctionWrapper2(_setter);

static UndefinedWrapper<
            Zebra
> _getter(Mammal src) {
  return src.oneOf1;
}
static void _setter(Mammal src, UndefinedWrapper<
            Zebra
> value) {
  src.oneOf1 = value;
}

@override
UndefinedWrapperReflection<
            Zebra
> get reflection => UndefinedWrapperReflection(
  XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                Zebra.$reflection
        

  ),
);

  UndefinedWrapper<
            Zebra
> example({
    required AggregatedDiscriminatorsResult discriminators,
    required Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ModelReflection>> discriminatorExampleResults,
    required Map<String, Uint8List>? fileCache,
  }) {
    if (discriminatorExampleResults.isEmpty) {
      //return undefined for non-first oneOfs.
      return UndefinedWrapper.undefined();
    } else {
      // if this reflection wasn't a result of any property, don't generate an example.

      if (!discriminatorExampleResults.values
          .any((e) => e.value == reflection.subReflection)) {
        // if there are no discriminator examples targetting the current class:
        return UndefinedWrapper.undefined();
      } else {
        // An example SHOULD be generated
      }
    }
    return reflection.example();
  }
}

class MammalOneOf2Part extends OneOfReflection<Mammal, 
            Pig
> {

  const MammalOneOf2Part({
  required MammalReflection Function() super.parentReflectionGetter,
});
@override
FunctionWrapper1<UndefinedWrapper<
            Pig
>, Mammal> get getter => FunctionWrapper1(_getter);
@override
FunctionWrapper2<void, Mammal, UndefinedWrapper<
            Pig
>> get setter => FunctionWrapper2(_setter);

static UndefinedWrapper<
            Pig
> _getter(Mammal src) {
  return src.oneOf2;
}
static void _setter(Mammal src, UndefinedWrapper<
            Pig
> value) {
  src.oneOf2 = value;
}

@override
UndefinedWrapperReflection<
            Pig
> get reflection => UndefinedWrapperReflection(
  XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                Pig.$reflection
        

  ),
);

  UndefinedWrapper<
            Pig
> example({
    required AggregatedDiscriminatorsResult discriminators,
    required Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ModelReflection>> discriminatorExampleResults,
    required Map<String, Uint8List>? fileCache,
  }) {
    if (discriminatorExampleResults.isEmpty) {
      //return undefined for non-first oneOfs.
      return UndefinedWrapper.undefined();
    } else {
      // if this reflection wasn't a result of any property, don't generate an example.

      if (!discriminatorExampleResults.values
          .any((e) => e.value == reflection.subReflection)) {
        // if there are no discriminator examples targetting the current class:
        return UndefinedWrapper.undefined();
      } else {
        // An example SHOULD be generated
      }
    }
    return reflection.example();
  }
}

