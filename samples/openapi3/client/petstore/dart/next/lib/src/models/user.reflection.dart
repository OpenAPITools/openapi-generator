// Model reflection

part of 'user.dart';


//class reflection

class UserReflection extends ModelReflection<User> {
  static UserReflection instanceGetter() => instance;
  static const instance = UserReflection._(
    modelName: r'User',
    className: r'User',
    xml: XmlReflection(
    xmlName: r'User',
),
    idPart: PropertyReflection<User, UndefinedWrapper<
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
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_idGetter),
      setter: FunctionWrapper2(_idSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forint
        
,
)
),
    ),
    usernamePart: PropertyReflection<User, UndefinedWrapper<
            String
>>(
      dartName: r'username',
      nullable: false,
      required: false,
      oasName: r'username',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_usernameGetter),
      setter: FunctionWrapper2(_usernameSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
),
    ),
    firstNamePart: PropertyReflection<User, UndefinedWrapper<
            String
>>(
      dartName: r'firstName',
      nullable: false,
      required: false,
      oasName: r'firstName',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_firstNameGetter),
      setter: FunctionWrapper2(_firstNameSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
),
    ),
    lastNamePart: PropertyReflection<User, UndefinedWrapper<
            String
>>(
      dartName: r'lastName',
      nullable: false,
      required: false,
      oasName: r'lastName',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_lastNameGetter),
      setter: FunctionWrapper2(_lastNameSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
),
    ),
    emailPart: PropertyReflection<User, UndefinedWrapper<
            String
>>(
      dartName: r'email',
      nullable: false,
      required: false,
      oasName: r'email',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_emailGetter),
      setter: FunctionWrapper2(_emailSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
),
    ),
    passwordPart: PropertyReflection<User, UndefinedWrapper<
            String
>>(
      dartName: r'password',
      nullable: false,
      required: false,
      oasName: r'password',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_passwordGetter),
      setter: FunctionWrapper2(_passwordSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
),
    ),
    phonePart: PropertyReflection<User, UndefinedWrapper<
            String
>>(
      dartName: r'phone',
      nullable: false,
      required: false,
      oasName: r'phone',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_phoneGetter),
      setter: FunctionWrapper2(_phoneSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
),
    ),
    userStatusPart: PropertyReflection<User, UndefinedWrapper<
            int
>>(
      dartName: r'userStatus',
      nullable: false,
      required: false,
      oasName: r'userStatus',
      oasType: r'integer',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_userStatusGetter),
      setter: FunctionWrapper2(_userStatusSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forint
        
,
)
),
    ),
    objectWithNoDeclaredPropsPart: PropertyReflection<User, UndefinedWrapper<
            $FreeFormObject
>>(
      dartName: r'objectWithNoDeclaredProps',
      nullable: false,
      required: false,
      oasName: r'objectWithNoDeclaredProps',
      oasType: r'object',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_objectWithNoDeclaredPropsGetter),
      setter: FunctionWrapper2(_objectWithNoDeclaredPropsSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.for$FreeFormObject
        
,
)
),
    ),
    objectWithNoDeclaredPropsNullablePart: PropertyReflection<User, UndefinedWrapper<
            $FreeFormObject
?>>(
      dartName: r'objectWithNoDeclaredPropsNullable',
      nullable: true,
      required: false,
      oasName: r'objectWithNoDeclaredPropsNullable',
      oasType: r'object',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_objectWithNoDeclaredPropsNullableGetter),
      setter: FunctionWrapper2(_objectWithNoDeclaredPropsNullableSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    NullableReflection(
            
        
        
            
                PrimitiveReflection.for$FreeFormObject
        
),
)
),
    ),
    anyTypePropPart: PropertyReflection<User, UndefinedWrapper<Object
?>>(
      dartName: r'anyTypeProp',
      nullable: true,
      required: false,
      oasName: r'anyTypeProp',
      oasType: r'Object',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_anyTypePropGetter),
      setter: FunctionWrapper2(_anyTypePropSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    NullableReflection(ObjectReflection()
),
)
),
    ),
    anyTypePropNullablePart: PropertyReflection<User, UndefinedWrapper<
            $FreeFormObject
?>>(
      dartName: r'anyTypePropNullable',
      nullable: true,
      required: false,
      oasName: r'anyTypePropNullable',
      oasType: r'object',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_anyTypePropNullableGetter),
      setter: FunctionWrapper2(_anyTypePropNullableSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    NullableReflection(
            
        
        
            
                PrimitiveReflection.for$FreeFormObject
        
),
)
),
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
  const UserReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.idPart,
    required this.usernamePart,
    required this.firstNamePart,
    required this.lastNamePart,
    required this.emailPart,
    required this.passwordPart,
    required this.phonePart,
    required this.userStatusPart,
    required this.objectWithNoDeclaredPropsPart,
    required this.objectWithNoDeclaredPropsNullablePart,
    required this.anyTypePropPart,
    required this.anyTypePropNullablePart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<User, UndefinedWrapper<
            int
>> idPart;
  static UndefinedWrapper<
            int
> _idGetter(User parent) {
    return parent.id;
  }
  static void _idSetter(User parent, UndefinedWrapper<
            int
> value) {
    parent.id = value;
  }

  final PropertyReflection<User, UndefinedWrapper<
            String
>> usernamePart;
  static UndefinedWrapper<
            String
> _usernameGetter(User parent) {
    return parent.username;
  }
  static void _usernameSetter(User parent, UndefinedWrapper<
            String
> value) {
    parent.username = value;
  }

  final PropertyReflection<User, UndefinedWrapper<
            String
>> firstNamePart;
  static UndefinedWrapper<
            String
> _firstNameGetter(User parent) {
    return parent.firstName;
  }
  static void _firstNameSetter(User parent, UndefinedWrapper<
            String
> value) {
    parent.firstName = value;
  }

  final PropertyReflection<User, UndefinedWrapper<
            String
>> lastNamePart;
  static UndefinedWrapper<
            String
> _lastNameGetter(User parent) {
    return parent.lastName;
  }
  static void _lastNameSetter(User parent, UndefinedWrapper<
            String
> value) {
    parent.lastName = value;
  }

  final PropertyReflection<User, UndefinedWrapper<
            String
>> emailPart;
  static UndefinedWrapper<
            String
> _emailGetter(User parent) {
    return parent.email;
  }
  static void _emailSetter(User parent, UndefinedWrapper<
            String
> value) {
    parent.email = value;
  }

  final PropertyReflection<User, UndefinedWrapper<
            String
>> passwordPart;
  static UndefinedWrapper<
            String
> _passwordGetter(User parent) {
    return parent.password;
  }
  static void _passwordSetter(User parent, UndefinedWrapper<
            String
> value) {
    parent.password = value;
  }

  final PropertyReflection<User, UndefinedWrapper<
            String
>> phonePart;
  static UndefinedWrapper<
            String
> _phoneGetter(User parent) {
    return parent.phone;
  }
  static void _phoneSetter(User parent, UndefinedWrapper<
            String
> value) {
    parent.phone = value;
  }

  final PropertyReflection<User, UndefinedWrapper<
            int
>> userStatusPart;
  static UndefinedWrapper<
            int
> _userStatusGetter(User parent) {
    return parent.userStatus;
  }
  static void _userStatusSetter(User parent, UndefinedWrapper<
            int
> value) {
    parent.userStatus = value;
  }

  final PropertyReflection<User, UndefinedWrapper<
            $FreeFormObject
>> objectWithNoDeclaredPropsPart;
  static UndefinedWrapper<
            $FreeFormObject
> _objectWithNoDeclaredPropsGetter(User parent) {
    return parent.objectWithNoDeclaredProps;
  }
  static void _objectWithNoDeclaredPropsSetter(User parent, UndefinedWrapper<
            $FreeFormObject
> value) {
    parent.objectWithNoDeclaredProps = value;
  }

  final PropertyReflection<User, UndefinedWrapper<
            $FreeFormObject
?>> objectWithNoDeclaredPropsNullablePart;
  static UndefinedWrapper<
            $FreeFormObject
?> _objectWithNoDeclaredPropsNullableGetter(User parent) {
    return parent.objectWithNoDeclaredPropsNullable;
  }
  static void _objectWithNoDeclaredPropsNullableSetter(User parent, UndefinedWrapper<
            $FreeFormObject
?> value) {
    parent.objectWithNoDeclaredPropsNullable = value;
  }

  final PropertyReflection<User, UndefinedWrapper<Object
?>> anyTypePropPart;
  static UndefinedWrapper<Object
?> _anyTypePropGetter(User parent) {
    return parent.anyTypeProp;
  }
  static void _anyTypePropSetter(User parent, UndefinedWrapper<Object
?> value) {
    parent.anyTypeProp = value;
  }

  final PropertyReflection<User, UndefinedWrapper<
            $FreeFormObject
?>> anyTypePropNullablePart;
  static UndefinedWrapper<
            $FreeFormObject
?> _anyTypePropNullableGetter(User parent) {
    return parent.anyTypePropNullable;
  }
  static void _anyTypePropNullableSetter(User parent, UndefinedWrapper<
            $FreeFormObject
?> value) {
    parent.anyTypePropNullable = value;
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
  List<PropertyReflection<User, dynamic>> get properties => [
    idPart,
usernamePart,
firstNamePart,
lastNamePart,
emailPart,
passwordPart,
phonePart,
userStatusPart,
objectWithNoDeclaredPropsPart,
objectWithNoDeclaredPropsNullablePart,
anyTypePropPart,
anyTypePropNullablePart,
  ];

  @override
  final AdditionalPropertiesPart<User, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(User instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(User instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<User, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  User empty() {
    return User(
    );
  }

  @override
  bool operator ==(Object other) {
    return other is UserReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


