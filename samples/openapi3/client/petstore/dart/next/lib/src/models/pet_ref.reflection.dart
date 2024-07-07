// Model reflection

part of 'pet_ref.dart';


//class reflection

class PetRefReflection extends ClassReflection<PetRef> {
  static PetRefReflection instanceGetter() => instance;
  static const instance = PetRefReflection._(
    modelName: r'PetRef',
    className: r'PetRef',
    idPart: PropertyReflection<PetRef, UndefinedWrapper<
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
    categoryPart: PropertyReflection<PetRef, UndefinedWrapper<
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
    namePart: PropertyReflection<PetRef, 
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
    photoUrlsPart: PropertyReflection<PetRef, 
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
      itemsReflection: ItemsReflection<PetRef, 
            String
>(parentReflectionGetter: instanceGetter,),
      getter: _photoUrlsGetter,
      setter: _photoUrlsSetter,
    ),
    tagsPart: PropertyReflection<PetRef, UndefinedWrapper<
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
      itemsReflection: ItemsReflection<PetRef, 
            Tag
>(parentReflectionGetter: instanceGetter,classReflection: TagReflection.instance,),
      getter: _tagsGetter,
      setter: _tagsSetter,
    ),
    statusPart: PropertyReflection<PetRef, UndefinedWrapper<
            PetRefStatusEnum
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
      itemsReflection: ItemsReflection<PetRef, Object
?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const PetRefReflection._({
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

  final PropertyReflection<PetRef, UndefinedWrapper<
            int
>> idPart;
  static UndefinedWrapper<
            int
> _idGetter(PetRef parent) {
    return parent.id;
  }
  static void _idSetter(PetRef parent, UndefinedWrapper<
            int
> value) {
    parent.id = value;
  }
  final PropertyReflection<PetRef, UndefinedWrapper<
            Category
>> categoryPart;
  static UndefinedWrapper<
            Category
> _categoryGetter(PetRef parent) {
    return parent.category;
  }
  static void _categorySetter(PetRef parent, UndefinedWrapper<
            Category
> value) {
    parent.category = value;
  }
  final PropertyReflection<PetRef, 
            String
> namePart;
  static 
            String
 _nameGetter(PetRef parent) {
    return parent.name;
  }
  static void _nameSetter(PetRef parent, 
            String
 value) {
    parent.name = value;
  }
  final PropertyReflection<PetRef, 
    List<
        
            String
>
> photoUrlsPart;
  static 
    List<
        
            String
>
 _photoUrlsGetter(PetRef parent) {
    return parent.photoUrls;
  }
  static void _photoUrlsSetter(PetRef parent, 
    List<
        
            String
>
 value) {
    parent.photoUrls = value;
  }
  final PropertyReflection<PetRef, UndefinedWrapper<
    List<
        
            Tag
>
>> tagsPart;
  static UndefinedWrapper<
    List<
        
            Tag
>
> _tagsGetter(PetRef parent) {
    return parent.tags;
  }
  static void _tagsSetter(PetRef parent, UndefinedWrapper<
    List<
        
            Tag
>
> value) {
    parent.tags = value;
  }
  final PropertyReflection<PetRef, UndefinedWrapper<
            PetRefStatusEnum
>> statusPart;
  static UndefinedWrapper<
            PetRefStatusEnum
> _statusGetter(PetRef parent) {
    return parent.status;
  }
  static void _statusSetter(PetRef parent, UndefinedWrapper<
            PetRefStatusEnum
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
  List<PropertyReflection<PetRef, dynamic>> get properties => [
    idPart,
categoryPart,
namePart,
photoUrlsPart,
tagsPart,
statusPart,
  ];

  final AdditionalPropertiesReflection<PetRef, Object
?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<PetRef, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<PetRef, dynamic>> get allOfs => [
    
  ];


  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => PetRef.canDeserialize(src);
  @override
  PetRef Function(Object? src) get deserializeFunction =>
      (src) => PetRef.deserialize(src);

  @override
  Object? Function(PetRef src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of PetRef.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  PetRef example({Map<String, (ClassReflection, PropertyReflection)> discriminators = const {}}) {
    final _reflection = this;
    if (discriminators.isEmpty) discriminators = _reflection.aggregatedDiscriminators;
    return PetRef(
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


            exampleEnum(PetRefStatusEnum.values)



);
      }(),
      additionalProperties: () { PartReflection? _partReflection = _reflection.additionalPropertiesPart; return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
      
    );
  }
}

class PetRefXmlReflection {
    const PetRefXmlReflection();
}

