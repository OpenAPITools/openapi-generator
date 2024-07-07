// Model reflection

part of 'all_of_model_array_any_of_all_of_link_list_column1_value.dart';


//class reflection

class AllOfModelArrayAnyOfAllOfLinkListColumn1ValueReflection extends ClassReflection<AllOfModelArrayAnyOfAllOfLinkListColumn1Value> {
  static AllOfModelArrayAnyOfAllOfLinkListColumn1ValueReflection instanceGetter() => instance;
  static const instance = AllOfModelArrayAnyOfAllOfLinkListColumn1ValueReflection._(
    modelName: r'AllOfModelArrayAnyOf_allOf_linkListColumn1_value',
    className: r'AllOfModelArrayAnyOfAllOfLinkListColumn1Value',
    
    
    anyOf0Part: AnyOfReflection(
      parentReflectionGetter: instanceGetter,
      classReflection: UserReflection.instance,
    ),
    
    anyOf1Part: AnyOfReflection(
      parentReflectionGetter: instanceGetter,
      classReflection: TagReflection.instance,
    ),
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<AllOfModelArrayAnyOfAllOfLinkListColumn1Value, Object
?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const AllOfModelArrayAnyOfAllOfLinkListColumn1ValueReflection._({
    required this.modelName,
    required this.className,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.anyOf0Part,
    
    required this.anyOf1Part,
    
    required this.additionalPropertiesPart,
  });




  @override
  final Map<String, ClassReflection> discriminatorMappings;
  @override
  final Map<String, ClassReflection> discriminatorImplicitMappings;
  @override
  final String? discriminatorKey;
  @override
  final String modelName;
  @override
  final String className;


  @override
  List<PropertyReflection<AllOfModelArrayAnyOfAllOfLinkListColumn1Value, dynamic>> get properties => [
      ];

  final AdditionalPropertiesReflection<AllOfModelArrayAnyOfAllOfLinkListColumn1Value, Object
?> additionalPropertiesPart;

  
  
  final AnyOfReflection<AllOfModelArrayAnyOfAllOfLinkListColumn1Value, 
            User
> anyOf0Part;
  
  final AnyOfReflection<AllOfModelArrayAnyOfAllOfLinkListColumn1Value, 
            Tag
> anyOf1Part;
  
  @override
  List<PartReflection<AllOfModelArrayAnyOfAllOfLinkListColumn1Value, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<AllOfModelArrayAnyOfAllOfLinkListColumn1Value, dynamic>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<AllOfModelArrayAnyOfAllOfLinkListColumn1Value, dynamic>> get oneOfs => [
    
  ];
  @override
  List<AnyOfReflection<AllOfModelArrayAnyOfAllOfLinkListColumn1Value, dynamic>> get anyOfs => [
    anyOf0Part,anyOf1Part,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => AllOfModelArrayAnyOfAllOfLinkListColumn1Value.canDeserialize(src);
  @override
  AllOfModelArrayAnyOfAllOfLinkListColumn1Value Function(Object? src) get deserializeFunction =>
      (src) => AllOfModelArrayAnyOfAllOfLinkListColumn1Value.deserialize(src);

  @override
  Object? Function(AllOfModelArrayAnyOfAllOfLinkListColumn1Value src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of AllOfModelArrayAnyOfAllOfLinkListColumn1Value.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  AllOfModelArrayAnyOfAllOfLinkListColumn1Value example({Map<String, (ClassReflection, PropertyReflection)> discriminators = const {}}) {
    final _reflection = this;
    if (discriminators.isEmpty) discriminators = _reflection.aggregatedDiscriminators;
    return AllOfModelArrayAnyOfAllOfLinkListColumn1Value(
      additionalProperties: () { PartReflection? _partReflection = _reflection.additionalPropertiesPart; return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
      
      anyOf0: () {
        PartReflection? _partReflection = _reflection.anyOf0Part;
        return UndefinedWrapper(


            
            


    User.$reflection.example(discriminators: discriminators)
    


);
      }(),
      anyOf1: () {
        PartReflection? _partReflection = _reflection.anyOf1Part;
        return UndefinedWrapper(


            
            


    Tag.$reflection.example(discriminators: discriminators)
    


);
      }(),
    );
  }
}

class AllOfModelArrayAnyOfAllOfLinkListColumn1ValueXmlReflection {
    const AllOfModelArrayAnyOfAllOfLinkListColumn1ValueXmlReflection();
}

