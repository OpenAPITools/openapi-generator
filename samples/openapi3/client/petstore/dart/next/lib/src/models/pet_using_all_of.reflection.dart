// Model reflection

part of 'pet_using_all_of.dart';


//class reflection

class PetUsingAllOfReflection extends ClassReflection<PetUsingAllOf> {
  static PetUsingAllOfReflection instanceGetter() => instance;
  static const instance = PetUsingAllOfReflection._(
    modelName: r'PetUsingAllOf',
    className: r'PetUsingAllOf',
    idPart: PropertyReflection<PetUsingAllOf, UndefinedWrapper<
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
    categoryPart: PropertyReflection<PetUsingAllOf, UndefinedWrapper<
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
    namePart: PropertyReflection<PetUsingAllOf, 
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
    photoUrlsPart: PropertyReflection<PetUsingAllOf, 
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
      itemsReflection: ItemsReflection<PetUsingAllOf, 
            String
>(parentReflectionGetter: instanceGetter,),
      getter: _photoUrlsGetter,
      setter: _photoUrlsSetter,
    ),
    tagsPart: PropertyReflection<PetUsingAllOf, UndefinedWrapper<
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
      itemsReflection: ItemsReflection<PetUsingAllOf, 
            Tag
>(parentReflectionGetter: instanceGetter,classReflection: TagReflection.instance,),
      getter: _tagsGetter,
      setter: _tagsSetter,
    ),
    statusPart: PropertyReflection<PetUsingAllOf, UndefinedWrapper<
            PetUsingAllOfStatusEnum
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
      itemsReflection: ItemsReflection<PetUsingAllOf, Object
?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const PetUsingAllOfReflection._({
    required this.modelName,
    required this.className,
    required this.idPart,
    required this.categoryPart,
    required this.namePart,
    required this.photoUrlsPart,
    required this.tagsPart,
    required this.statusPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<PetUsingAllOf, UndefinedWrapper<
            int
>> idPart;
  static UndefinedWrapper<
            int
> _idGetter(PetUsingAllOf parent) {
    return parent.id;
  }
  static void _idSetter(PetUsingAllOf parent, UndefinedWrapper<
            int
> value) {
    parent.id = value;
  }
  final PropertyReflection<PetUsingAllOf, UndefinedWrapper<
            Category
>> categoryPart;
  static UndefinedWrapper<
            Category
> _categoryGetter(PetUsingAllOf parent) {
    return parent.category;
  }
  static void _categorySetter(PetUsingAllOf parent, UndefinedWrapper<
            Category
> value) {
    parent.category = value;
  }
  final PropertyReflection<PetUsingAllOf, 
            String
> namePart;
  static 
            String
 _nameGetter(PetUsingAllOf parent) {
    return parent.name;
  }
  static void _nameSetter(PetUsingAllOf parent, 
            String
 value) {
    parent.name = value;
  }
  final PropertyReflection<PetUsingAllOf, 
    List<
        
            String
>
> photoUrlsPart;
  static 
    List<
        
            String
>
 _photoUrlsGetter(PetUsingAllOf parent) {
    return parent.photoUrls;
  }
  static void _photoUrlsSetter(PetUsingAllOf parent, 
    List<
        
            String
>
 value) {
    parent.photoUrls = value;
  }
  final PropertyReflection<PetUsingAllOf, UndefinedWrapper<
    List<
        
            Tag
>
>> tagsPart;
  static UndefinedWrapper<
    List<
        
            Tag
>
> _tagsGetter(PetUsingAllOf parent) {
    return parent.tags;
  }
  static void _tagsSetter(PetUsingAllOf parent, UndefinedWrapper<
    List<
        
            Tag
>
> value) {
    parent.tags = value;
  }
  final PropertyReflection<PetUsingAllOf, UndefinedWrapper<
            PetUsingAllOfStatusEnum
>> statusPart;
  static UndefinedWrapper<
            PetUsingAllOfStatusEnum
> _statusGetter(PetUsingAllOf parent) {
    return parent.status;
  }
  static void _statusSetter(PetUsingAllOf parent, UndefinedWrapper<
            PetUsingAllOfStatusEnum
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
  List<PropertyReflection<PetUsingAllOf, dynamic>> get properties => [
    idPart,
categoryPart,
namePart,
photoUrlsPart,
tagsPart,
statusPart,
  ];

  final AdditionalPropertiesReflection<PetUsingAllOf, Object
?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<PetUsingAllOf, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<PetUsingAllOf, dynamic>> get allOfs => [
    
  ];


  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => PetUsingAllOf.canDeserialize(src);
  @override
  PetUsingAllOf Function(Object? src) get deserializeFunction =>
      (src) => PetUsingAllOf.deserialize(src);

  @override
  Object? Function(PetUsingAllOf src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of PetUsingAllOf.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  PetUsingAllOf example({Map<String, (ClassReflection, PropertyReflection)> discriminators = const {}}) {
    final _reflection = this;
    if (discriminators.isEmpty) discriminators = _reflection.aggregatedDiscriminators;
    return PetUsingAllOf(
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
      photoUrls: () {
        PartReflection? _partReflection = _reflection.photoUrlsPart;
        
        return 


    exampleList(() { _partReflection = _partReflection?.itemsReflection; return 


            
            


    
    exampleString()


; })



;
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


            exampleEnum(PetUsingAllOfStatusEnum.values)



);
      }(),
      additionalProperties: () { PartReflection? _partReflection = _reflection.additionalPropertiesPart; return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
      
    );
  }
}

class PetUsingAllOfXmlReflection {
    const PetUsingAllOfXmlReflection();
}

