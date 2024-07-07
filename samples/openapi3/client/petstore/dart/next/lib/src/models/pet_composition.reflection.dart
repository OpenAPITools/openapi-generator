// Model reflection

part of 'pet_composition.dart';


//class reflection

class PetCompositionReflection extends ClassReflection<PetComposition> {
  static PetCompositionReflection instanceGetter() => instance;
  static const instance = PetCompositionReflection._(
    modelName: r'PetComposition',
    className: r'PetComposition',
    photoUrlsPart: PropertyReflection<PetComposition, 
    List<
        
            String
>
>(
      dartName: r'photoUrls',
      nullable: false,
      required: true,
      oasName: r'photoUrls',
      oasType: r'array',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      itemsReflection: ItemsReflection<PetComposition, 
            String
>(parentReflectionGetter: instanceGetter,),
      getter: _photoUrlsGetter,
      setter: _photoUrlsSetter,
    ),
    namePart: PropertyReflection<PetComposition, 
            String
>(
      dartName: r'name',
      nullable: false,
      required: true,
      oasName: r'name',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _nameGetter,
      setter: _nameSetter,
    ),
    idPart: PropertyReflection<PetComposition, UndefinedWrapper<
            int
>>(
      dartName: r'id',
      nullable: false,
      required: false,
      oasName: r'id',
      oasType: r'integer',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _idGetter,
      setter: _idSetter,
    ),
    categoryPart: PropertyReflection<PetComposition, UndefinedWrapper<
            Category
>>(
      dartName: r'category',
      nullable: false,
      required: false,
      oasName: r'category',
      oasType: r'Category',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      classReflection: CategoryReflection.instance,
      getter: _categoryGetter,
      setter: _categorySetter,
    ),
    tagsPart: PropertyReflection<PetComposition, UndefinedWrapper<
    List<
        
            Tag
>
>>(
      dartName: r'tags',
      nullable: false,
      required: false,
      oasName: r'tags',
      oasType: r'array',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      itemsReflection: ItemsReflection<PetComposition, 
            Tag
>(parentReflectionGetter: instanceGetter,classReflection: TagReflection.instance,),
      getter: _tagsGetter,
      setter: _tagsSetter,
    ),
    statusPart: PropertyReflection<PetComposition, UndefinedWrapper<
            PetStatusEnum
>>(
      dartName: r'status',
      nullable: false,
      required: false,
      oasName: r'status',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _statusGetter,
      setter: _statusSetter,
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<PetComposition, Object
?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const PetCompositionReflection._({
    required this.modelName,
    required this.className,
    required this.photoUrlsPart,
    required this.namePart,
    required this.idPart,
    required this.categoryPart,
    required this.tagsPart,
    required this.statusPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<PetComposition, 
    List<
        
            String
>
> photoUrlsPart;
  static 
    List<
        
            String
>
 _photoUrlsGetter(PetComposition parent) {
    return parent.photoUrls;
  }
  static void _photoUrlsSetter(PetComposition parent, 
    List<
        
            String
>
 value) {
    parent.photoUrls = value;
  }
  final PropertyReflection<PetComposition, 
            String
> namePart;
  static 
            String
 _nameGetter(PetComposition parent) {
    return parent.name;
  }
  static void _nameSetter(PetComposition parent, 
            String
 value) {
    parent.name = value;
  }
  final PropertyReflection<PetComposition, UndefinedWrapper<
            int
>> idPart;
  static UndefinedWrapper<
            int
> _idGetter(PetComposition parent) {
    return parent.id;
  }
  static void _idSetter(PetComposition parent, UndefinedWrapper<
            int
> value) {
    parent.id = value;
  }
  final PropertyReflection<PetComposition, UndefinedWrapper<
            Category
>> categoryPart;
  static UndefinedWrapper<
            Category
> _categoryGetter(PetComposition parent) {
    return parent.category;
  }
  static void _categorySetter(PetComposition parent, UndefinedWrapper<
            Category
> value) {
    parent.category = value;
  }
  final PropertyReflection<PetComposition, UndefinedWrapper<
    List<
        
            Tag
>
>> tagsPart;
  static UndefinedWrapper<
    List<
        
            Tag
>
> _tagsGetter(PetComposition parent) {
    return parent.tags;
  }
  static void _tagsSetter(PetComposition parent, UndefinedWrapper<
    List<
        
            Tag
>
> value) {
    parent.tags = value;
  }
  final PropertyReflection<PetComposition, UndefinedWrapper<
            PetStatusEnum
>> statusPart;
  static UndefinedWrapper<
            PetStatusEnum
> _statusGetter(PetComposition parent) {
    return parent.status;
  }
  static void _statusSetter(PetComposition parent, UndefinedWrapper<
            PetStatusEnum
> value) {
    parent.status = value;
  }



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
  List<PropertyReflection<PetComposition, dynamic>> get properties => [
    photoUrlsPart,
namePart,
idPart,
categoryPart,
tagsPart,
statusPart,
  ];

  final AdditionalPropertiesReflection<PetComposition, Object
?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<PetComposition, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<PetComposition, dynamic>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<PetComposition, dynamic>> get oneOfs => [
    
  ];
  @override
  List<AnyOfReflection<PetComposition, dynamic>> get anyOfs => [
    
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => PetComposition.canDeserialize(src);
  @override
  PetComposition Function(Object? src) get deserializeFunction =>
      (src) => PetComposition.deserialize(src);

  @override
  Object? Function(PetComposition src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of PetComposition.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  PetComposition example({Map<String, (ClassReflection, PropertyReflection)> discriminators = const {}}) {
    final _reflection = this;
    if (discriminators.isEmpty) discriminators = _reflection.aggregatedDiscriminators;
    return PetComposition(
      photoUrls: () {
        PartReflection? _partReflection = _reflection.photoUrlsPart;
        
        return 


    exampleList(() { _partReflection = _partReflection?.itemsReflection; return 


            
            


    
    exampleString()


; })



;
      }(),
      name: () {
        PartReflection? _partReflection = _reflection.namePart;
        
        final disc = discriminators[r'name'];
        if (disc != null) {
          final result = exampleDiscriminator(_partReflection, disc);
          if (result != null) {
            return result;
          }
        }
        
        return 


            
            


    
    exampleString()


;
      }(),
      id: () {
        PartReflection? _partReflection = _reflection.idPart;
        
        return UndefinedWrapper(


            
            


    
    exampleint()


);
      }(),
      category: () {
        PartReflection? _partReflection = _reflection.categoryPart;
        
        return UndefinedWrapper(


            
            


    Category.$reflection.example()
    


);
      }(),
      tags: () {
        PartReflection? _partReflection = _reflection.tagsPart;
        
        return UndefinedWrapper(


    exampleList(() { _partReflection = _partReflection?.itemsReflection; return 


            
            


    Tag.$reflection.example()
    


; })



);
      }(),
      status: () {
        PartReflection? _partReflection = _reflection.statusPart;
        
        return UndefinedWrapper(


            exampleEnum(PetStatusEnum.values)



);
      }(),
      additionalProperties: () { PartReflection? _partReflection = _reflection.additionalPropertiesPart; return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
      
    );
  }
}

class PetCompositionXmlReflection {
    const PetCompositionXmlReflection();
}

