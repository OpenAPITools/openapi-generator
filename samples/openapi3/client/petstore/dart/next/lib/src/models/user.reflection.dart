// Model reflection

part of 'user.dart';


//class reflection

class UserReflection extends ClassReflection<User> {
  static UserReflection instanceGetter() => instance;
  static const instance = UserReflection._(
    modelName: r'User',
    className: r'User',
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
      getter: _idGetter,
      setter: _idSetter,
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
      getter: _usernameGetter,
      setter: _usernameSetter,
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
      getter: _firstNameGetter,
      setter: _firstNameSetter,
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
      getter: _lastNameGetter,
      setter: _lastNameSetter,
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
      getter: _emailGetter,
      setter: _emailSetter,
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
      getter: _passwordGetter,
      setter: _passwordSetter,
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
      getter: _phoneGetter,
      setter: _phoneSetter,
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
      getter: _userStatusGetter,
      setter: _userStatusSetter,
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
      getter: _objectWithNoDeclaredPropsGetter,
      setter: _objectWithNoDeclaredPropsSetter,
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
      getter: _objectWithNoDeclaredPropsNullableGetter,
      setter: _objectWithNoDeclaredPropsNullableSetter,
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
      getter: _anyTypePropGetter,
      setter: _anyTypePropSetter,
    ),
    anyTypePropNullablePart: PropertyReflection<User, UndefinedWrapper<Object

?>>(
      dartName: r'anyTypePropNullable',
      nullable: true,
      required: false,
      oasName: r'anyTypePropNullable',
      oasType: r'Object',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _anyTypePropNullableGetter,
      setter: _anyTypePropNullableSetter,
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<User, Object

?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const UserReflection._({
    required this.modelName,
    required this.className,
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
  final PropertyReflection<User, UndefinedWrapper<Object

?>> anyTypePropNullablePart;
  static UndefinedWrapper<Object

?> _anyTypePropNullableGetter(User parent) {
    return parent.anyTypePropNullable;
  }
  static void _anyTypePropNullableSetter(User parent, UndefinedWrapper<Object

?> value) {
    parent.anyTypePropNullable = value;
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

  final AdditionalPropertiesReflection<User, Object

?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<User, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<User, dynamic>> get allOfs => [
    
  ];


  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => User.canDeserialize(src);
  @override
  User Function(Object? src) get deserializeFunction =>
      (src) => User.deserialize(src);

  @override
  Object? Function(User src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of User.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  User example({AggregatedDiscriminatorsResult? discriminators, Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>>
        discriminatorExampleResults = const {},}) {
    final _reflection = this;
    final actualDiscriminators = discriminators ?? _reflection.aggregatedDiscriminators;
    discriminatorExampleResults = Map.from(discriminatorExampleResults);
    for (final MapEntry(key: propName, value: mappings) in actualDiscriminators.entries) {
      if (discriminatorExampleResults.containsKey(propName)) {
        continue;
      }
      final r =  exampleDiscriminator(mappings);
      if (r != null){
        discriminatorExampleResults[propName] = r;
      }
    }

    final exampleResult = User(
      id: () {
        var result = 


            
            


    
    exampleint()


;
        return UndefinedWrapper(result);
      } (),
      username: () {
        var result = 


            
            


    
    exampleString()


;
        final preSelectedResult = discriminatorExampleResults[usernamePart.oasName]?.key.key;
        if (preSelectedResult != null) {
          result = preSelectedResult;
        }
        return UndefinedWrapper(result);
      } (),
      firstName: () {
        var result = 


            
            


    
    exampleString()


;
        final preSelectedResult = discriminatorExampleResults[firstNamePart.oasName]?.key.key;
        if (preSelectedResult != null) {
          result = preSelectedResult;
        }
        return UndefinedWrapper(result);
      } (),
      lastName: () {
        var result = 


            
            


    
    exampleString()


;
        final preSelectedResult = discriminatorExampleResults[lastNamePart.oasName]?.key.key;
        if (preSelectedResult != null) {
          result = preSelectedResult;
        }
        return UndefinedWrapper(result);
      } (),
      email: () {
        var result = 


            
            


    
    exampleString()


;
        final preSelectedResult = discriminatorExampleResults[emailPart.oasName]?.key.key;
        if (preSelectedResult != null) {
          result = preSelectedResult;
        }
        return UndefinedWrapper(result);
      } (),
      password: () {
        var result = 


            
            


    
    exampleString()


;
        final preSelectedResult = discriminatorExampleResults[passwordPart.oasName]?.key.key;
        if (preSelectedResult != null) {
          result = preSelectedResult;
        }
        return UndefinedWrapper(result);
      } (),
      phone: () {
        var result = 


            
            


    
    exampleString()


;
        final preSelectedResult = discriminatorExampleResults[phonePart.oasName]?.key.key;
        if (preSelectedResult != null) {
          result = preSelectedResult;
        }
        return UndefinedWrapper(result);
      } (),
      userStatus: () {
        var result = 


            
            


    
    exampleint()


;
        return UndefinedWrapper(result);
      } (),
      objectWithNoDeclaredProps: () {
        var result = 


            
            


    
    example$FreeFormObject()


;
        return UndefinedWrapper(result);
      } (),
      objectWithNoDeclaredPropsNullable: () {
        var result = exampleNullable(() =>


            
            


    
    example$FreeFormObject()


 ) ;
        return UndefinedWrapper(result);
      } (),
      anyTypeProp: () {
        var result = exampleNullable(() =>

exampleObject()



 ) ;
        return UndefinedWrapper(result);
      } (),
      anyTypePropNullable: () {
        var result = exampleNullable(() =>

exampleObject()



 ) ;
        return UndefinedWrapper(result);
      } (),
      additionalProperties: () { return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
    );
    
    return exampleResult;
  }
}


class UserXmlReflection {
    const UserXmlReflection();
}

