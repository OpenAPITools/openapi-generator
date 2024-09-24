// Model reflection

part of 'gm_fruit.dart';


//class reflection

class GmFruitReflection extends ModelReflection<GmFruit> {
  static GmFruitReflection instanceGetter() => instance;
  static const instance = GmFruitReflection._(
    modelName: r'gmFruit',
    className: r'GmFruit',
    xml: XmlReflection(
),
    colorPart: PropertyReflection<GmFruit, UndefinedWrapper<
            String
>>(
      dartName: r'color',
      nullable: false,
      required: false,
      oasName: r'color',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_colorGetter),
      setter: FunctionWrapper2(_colorSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
),
    ),
    
    
    anyOf0Part: GmFruitAnyOf0Part(
      parentReflectionGetter: instanceGetter,
    ),
    
    anyOf1Part: GmFruitAnyOf1Part(
      parentReflectionGetter: instanceGetter,
    ),
    
  );
  const GmFruitReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.colorPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.anyOf0Part,
    
    required this.anyOf1Part,
    
  });

  final PropertyReflection<GmFruit, UndefinedWrapper<
            String
>> colorPart;
  static UndefinedWrapper<
            String
> _colorGetter(GmFruit parent) {
    return parent.color;
  }
  static void _colorSetter(GmFruit parent, UndefinedWrapper<
            String
> value) {
    parent.color = value;
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
  List<PropertyReflection<GmFruit, dynamic>> get properties => [
    colorPart,
  ];


  
  
  final GmFruitAnyOf0Part anyOf0Part;
  
  final GmFruitAnyOf1Part anyOf1Part;
  

  @override
  List<AllOfReflection<GmFruit, Object>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<GmFruit, Object>> get oneOfs => [
    
  ];
  @override
  List<AnyOfReflection<GmFruit, Object>> get anyOfs => [
    anyOf0Part,anyOf1Part,
  ];


  /// Creates an empty instance used as a starting point for deserialization.
  @override
  GmFruit empty() {
    return GmFruit(
    );
  }

  @override
  bool operator ==(Object other) {
    return other is GmFruitReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}

class GmFruitAnyOf0Part extends AnyOfReflection<GmFruit, 
            Apple
> {

  const GmFruitAnyOf0Part({
  required GmFruitReflection Function() super.parentReflectionGetter,
});
@override
FunctionWrapper1<UndefinedWrapper<
            Apple
>, GmFruit> get getter => FunctionWrapper1(_getter);
@override
FunctionWrapper2<void, GmFruit, UndefinedWrapper<
            Apple
>> get setter => FunctionWrapper2(_setter);

static UndefinedWrapper<
            Apple
> _getter(GmFruit src) {
  return src.anyOf0;
}
static void _setter(GmFruit src, UndefinedWrapper<
            Apple
> value) {
  src.anyOf0 = value;
}

@override
UndefinedWrapperReflection<
            Apple
> get reflection => UndefinedWrapperReflection(
  XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                Apple.$reflection
        

  ),
);

  UndefinedWrapper<
            Apple
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
class GmFruitAnyOf1Part extends AnyOfReflection<GmFruit, 
            Banana
> {

  const GmFruitAnyOf1Part({
  required GmFruitReflection Function() super.parentReflectionGetter,
});
@override
FunctionWrapper1<UndefinedWrapper<
            Banana
>, GmFruit> get getter => FunctionWrapper1(_getter);
@override
FunctionWrapper2<void, GmFruit, UndefinedWrapper<
            Banana
>> get setter => FunctionWrapper2(_setter);

static UndefinedWrapper<
            Banana
> _getter(GmFruit src) {
  return src.anyOf1;
}
static void _setter(GmFruit src, UndefinedWrapper<
            Banana
> value) {
  src.anyOf1 = value;
}

@override
UndefinedWrapperReflection<
            Banana
> get reflection => UndefinedWrapperReflection(
  XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                Banana.$reflection
        

  ),
);

  UndefinedWrapper<
            Banana
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

