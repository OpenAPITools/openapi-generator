// Model reflection

part of 'fruit.dart';


//class reflection

class FruitReflection extends ModelReflection<Fruit> {
  static FruitReflection instanceGetter() => instance;
  static const instance = FruitReflection._(
    modelName: r'fruit',
    className: r'Fruit',
    xml: XmlReflection(
),
    colorPart: PropertyReflection<Fruit, UndefinedWrapper<
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
    
    
    oneOf0Part: FruitOneOf0Part(
      parentReflectionGetter: instanceGetter,
    ),
    
    oneOf1Part: FruitOneOf1Part(
      parentReflectionGetter: instanceGetter,
    ),
    
  );
  const FruitReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.colorPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.oneOf0Part,
    
    required this.oneOf1Part,
    
  });

  final PropertyReflection<Fruit, UndefinedWrapper<
            String
>> colorPart;
  static UndefinedWrapper<
            String
> _colorGetter(Fruit parent) {
    return parent.color;
  }
  static void _colorSetter(Fruit parent, UndefinedWrapper<
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
  List<PropertyReflection<Fruit, dynamic>> get properties => [
    colorPart,
  ];


  
  
  final FruitOneOf0Part oneOf0Part;
  
  final FruitOneOf1Part oneOf1Part;
  

  @override
  List<AllOfReflection<Fruit, Object>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<Fruit, Object>> get oneOfs => [
    oneOf0Part,oneOf1Part,
  ];
  @override
  List<AnyOfReflection<Fruit, Object>> get anyOfs => [
    
  ];


  /// Creates an empty instance used as a starting point for deserialization.
  @override
  Fruit empty() {
    return Fruit(
    );
  }

  @override
  bool operator ==(Object other) {
    return other is FruitReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


class FruitOneOf0Part extends OneOfReflection<Fruit, 
            Apple
> {

  const FruitOneOf0Part({
  required FruitReflection Function() super.parentReflectionGetter,
});
@override
FunctionWrapper1<UndefinedWrapper<
            Apple
>, Fruit> get getter => FunctionWrapper1(_getter);
@override
FunctionWrapper2<void, Fruit, UndefinedWrapper<
            Apple
>> get setter => FunctionWrapper2(_setter);

static UndefinedWrapper<
            Apple
> _getter(Fruit src) {
  return src.oneOf0;
}
static void _setter(Fruit src, UndefinedWrapper<
            Apple
> value) {
  src.oneOf0 = value;
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

class FruitOneOf1Part extends OneOfReflection<Fruit, 
            Banana
> {

  const FruitOneOf1Part({
  required FruitReflection Function() super.parentReflectionGetter,
});
@override
FunctionWrapper1<UndefinedWrapper<
            Banana
>, Fruit> get getter => FunctionWrapper1(_getter);
@override
FunctionWrapper2<void, Fruit, UndefinedWrapper<
            Banana
>> get setter => FunctionWrapper2(_setter);

static UndefinedWrapper<
            Banana
> _getter(Fruit src) {
  return src.oneOf1;
}
static void _setter(Fruit src, UndefinedWrapper<
            Banana
> value) {
  src.oneOf1 = value;
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

