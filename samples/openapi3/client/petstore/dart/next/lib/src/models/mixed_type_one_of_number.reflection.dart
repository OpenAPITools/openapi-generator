// Model reflection

part of 'mixed_type_one_of_number.dart';


//class reflection

class MixedTypeOneOfNumberReflection extends ModelReflection<MixedTypeOneOfNumber> {
  static MixedTypeOneOfNumberReflection instanceGetter() => instance;
  static const instance = MixedTypeOneOfNumberReflection._(
    modelName: r'MixedTypeOneOfNumber',
    className: r'MixedTypeOneOfNumber',
    xml: XmlReflection(
),
    
    
    oneOf0Part: MixedTypeOneOfNumberOneOf0Part(
      parentReflectionGetter: instanceGetter,
    ),
    
    oneOf1Part: MixedTypeOneOfNumberOneOf1Part(
      parentReflectionGetter: instanceGetter,
    ),
    
  );
  const MixedTypeOneOfNumberReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.oneOf0Part,
    
    required this.oneOf1Part,
    
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
  List<PropertyReflection<MixedTypeOneOfNumber, dynamic>> get properties => [
      ];


  
  
  final MixedTypeOneOfNumberOneOf0Part oneOf0Part;
  
  final MixedTypeOneOfNumberOneOf1Part oneOf1Part;
  

  @override
  List<AllOfReflection<MixedTypeOneOfNumber, Object>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<MixedTypeOneOfNumber, Object>> get oneOfs => [
    oneOf0Part,oneOf1Part,
  ];
  @override
  List<AnyOfReflection<MixedTypeOneOfNumber, Object>> get anyOfs => [
    
  ];


  /// Creates an empty instance used as a starting point for deserialization.
  @override
  MixedTypeOneOfNumber empty() {
    return MixedTypeOneOfNumber(
    );
  }

  @override
  bool operator ==(Object other) {
    return other is MixedTypeOneOfNumberReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


class MixedTypeOneOfNumberOneOf0Part extends OneOfReflection<MixedTypeOneOfNumber, 
            double
> {

  const MixedTypeOneOfNumberOneOf0Part({
  required MixedTypeOneOfNumberReflection Function() super.parentReflectionGetter,
});
@override
FunctionWrapper1<UndefinedWrapper<
            double
>, MixedTypeOneOfNumber> get getter => FunctionWrapper1(_getter);
@override
FunctionWrapper2<void, MixedTypeOneOfNumber, UndefinedWrapper<
            double
>> get setter => FunctionWrapper2(_setter);

static UndefinedWrapper<
            double
> _getter(MixedTypeOneOfNumber src) {
  return src.oneOf0;
}
static void _setter(MixedTypeOneOfNumber src, UndefinedWrapper<
            double
> value) {
  src.oneOf0 = value;
}

@override
UndefinedWrapperReflection<
            double
> get reflection => UndefinedWrapperReflection(
  XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.fordouble
        

  ),
);

  UndefinedWrapper<
            double
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

class MixedTypeOneOfNumberOneOf1Part extends OneOfReflection<MixedTypeOneOfNumber, 
            double
> {

  const MixedTypeOneOfNumberOneOf1Part({
  required MixedTypeOneOfNumberReflection Function() super.parentReflectionGetter,
});
@override
FunctionWrapper1<UndefinedWrapper<
            double
>, MixedTypeOneOfNumber> get getter => FunctionWrapper1(_getter);
@override
FunctionWrapper2<void, MixedTypeOneOfNumber, UndefinedWrapper<
            double
>> get setter => FunctionWrapper2(_setter);

static UndefinedWrapper<
            double
> _getter(MixedTypeOneOfNumber src) {
  return src.oneOf1;
}
static void _setter(MixedTypeOneOfNumber src, UndefinedWrapper<
            double
> value) {
  src.oneOf1 = value;
}

@override
UndefinedWrapperReflection<
            double
> get reflection => UndefinedWrapperReflection(
  XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.fordouble
        

  ),
);

  UndefinedWrapper<
            double
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

