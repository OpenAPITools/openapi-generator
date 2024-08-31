// Model reflection

part of 'pig.dart';


//class reflection

class PigReflection extends ModelReflection<Pig> {
  static PigReflection instanceGetter() => instance;
  static const instance = PigReflection._(
    modelName: r'Pig',
    className: r'Pig',
    xml: const XmlReflection(
),
    classNamePart: PropertyReflection<Pig, 
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
      xml: const XmlReflection(
),
      getter: FunctionWrapper1(_classNameGetter),
      setter: FunctionWrapper2(_classNameSetter),
      reflection: 
            
        
        
            
                PrimitiveReflection.forString
        
,
    ),
    discriminatorKey: r'className',
    discriminatorImplicitMappings: const {
      r'BasquePig': BasquePigReflection.instance,
      r'DanishPig': DanishPigReflection.instance,
    },
    discriminatorMappings: const {
      r'BasquePig': BasquePigReflection.instance,
      r'DanishPig': DanishPigReflection.instance,
    },
    
    
    oneOf0Part: PigOneOf0Part(
      parentReflectionGetter: instanceGetter,
    ),
    
    oneOf1Part: PigOneOf1Part(
      parentReflectionGetter: instanceGetter,
    ),
    
    additionalPropertiesPart: AdditionalPropertiesPart(
      parentReflectionGetter: instanceGetter,
      itemReflection: NullableReflection(ObjectReflection()
),
      getter: FunctionWrapper1(_AdditionalPropertiesGetter),
      setter: FunctionWrapper2(_AdditionalPropertiesSetter),
    ),
  );
  const PigReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.classNamePart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.oneOf0Part,
    
    required this.oneOf1Part,
    
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<Pig, 
            String
> classNamePart;
  static 
            String
 _classNameGetter(Pig parent) {
    return parent.className;
  }
  static void _classNameSetter(Pig parent, 
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
  List<PropertyReflection<Pig, dynamic>> get properties => [
    classNamePart,
  ];

  @override
  final AdditionalPropertiesPart<Pig, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(Pig instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(Pig instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  
  final PigOneOf0Part oneOf0Part;
  
  final PigOneOf1Part oneOf1Part;
  

  @override
  List<AllOfReflection<Pig, dynamic>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<Pig, dynamic>> get oneOfs => [
    oneOf0Part,oneOf1Part,
  ];
  @override
  List<AnyOfReflection<Pig, dynamic>> get anyOfs => [
    
  ];


  /// Creates an empty instance used as a starting point for deserialization.
  @override
  Pig empty() {
    return Pig(
      className: classNamePart.reflection.emptyFunction(),
    );
  }
}


class PigOneOf0Part extends OneOfReflection<Pig, 
            BasquePig
> {

  const PigOneOf0Part({
  required PigReflection Function() super.parentReflectionGetter,
});
@override
FunctionWrapper1<UndefinedWrapper<
            BasquePig
>, Pig> get getter => FunctionWrapper1(_getter);
@override
FunctionWrapper2<void, Pig, UndefinedWrapper<
            BasquePig
>> get setter => FunctionWrapper2(_setter);

static UndefinedWrapper<
            BasquePig
> _getter(Pig src) {
  return src.oneOf0;
}
static void _setter(Pig src, UndefinedWrapper<
            BasquePig
> value) {
  src.oneOf0 = value;
}

@override
UndefinedWrapperReflection<
            BasquePig
> get reflection => UndefinedWrapperReflection(
            
        
        
            
                BasquePig.$reflection
        
);

  UndefinedWrapper<
            BasquePig
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

class PigOneOf1Part extends OneOfReflection<Pig, 
            DanishPig
> {

  const PigOneOf1Part({
  required PigReflection Function() super.parentReflectionGetter,
});
@override
FunctionWrapper1<UndefinedWrapper<
            DanishPig
>, Pig> get getter => FunctionWrapper1(_getter);
@override
FunctionWrapper2<void, Pig, UndefinedWrapper<
            DanishPig
>> get setter => FunctionWrapper2(_setter);

static UndefinedWrapper<
            DanishPig
> _getter(Pig src) {
  return src.oneOf1;
}
static void _setter(Pig src, UndefinedWrapper<
            DanishPig
> value) {
  src.oneOf1 = value;
}

@override
UndefinedWrapperReflection<
            DanishPig
> get reflection => UndefinedWrapperReflection(
            
        
        
            
                DanishPig.$reflection
        
);

  UndefinedWrapper<
            DanishPig
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

