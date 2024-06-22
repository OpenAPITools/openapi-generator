// Model reflection

part of 'user.dart';


//class reflection

class UserReflection extends ClassReflection<User> {
  static const instance = UserReflection._(
    id: PropertyReflection(
      dartName: r'id',
      nullable: false,
      required: false,
      oasName: r'id',
      oasType: r'integer',
      pattern: null,
    ),
    username: PropertyReflection(
      dartName: r'username',
      nullable: false,
      required: false,
      oasName: r'username',
      oasType: r'string',
      pattern: null,
    ),
    firstName: PropertyReflection(
      dartName: r'firstName',
      nullable: false,
      required: false,
      oasName: r'firstName',
      oasType: r'string',
      pattern: null,
    ),
    lastName: PropertyReflection(
      dartName: r'lastName',
      nullable: false,
      required: false,
      oasName: r'lastName',
      oasType: r'string',
      pattern: null,
    ),
    email: PropertyReflection(
      dartName: r'email',
      nullable: false,
      required: false,
      oasName: r'email',
      oasType: r'string',
      pattern: null,
    ),
    password: PropertyReflection(
      dartName: r'password',
      nullable: false,
      required: false,
      oasName: r'password',
      oasType: r'string',
      pattern: null,
    ),
    phone: PropertyReflection(
      dartName: r'phone',
      nullable: false,
      required: false,
      oasName: r'phone',
      oasType: r'string',
      pattern: null,
    ),
    userStatus: PropertyReflection(
      dartName: r'userStatus',
      nullable: false,
      required: false,
      oasName: r'userStatus',
      oasType: r'integer',
      pattern: null,
    ),
    objectWithNoDeclaredProps: PropertyReflection(
      dartName: r'objectWithNoDeclaredProps',
      nullable: false,
      required: false,
      oasName: r'objectWithNoDeclaredProps',
      oasType: r'object',
      pattern: null,
    ),
    objectWithNoDeclaredPropsNullable: PropertyReflection(
      dartName: r'objectWithNoDeclaredPropsNullable',
      nullable: true,
      required: false,
      oasName: r'objectWithNoDeclaredPropsNullable',
      oasType: r'object',
      pattern: null,
    ),
    anyTypeProp: PropertyReflection(
      dartName: r'anyTypeProp',
      nullable: true,
      required: false,
      oasName: r'anyTypeProp',
      oasType: r'Object',
      pattern: null,
    ),
    anyTypePropNullable: PropertyReflection(
      dartName: r'anyTypePropNullable',
      nullable: true,
      required: false,
      oasName: r'anyTypePropNullable',
      oasType: r'Object',
      pattern: null,
    ),
  );
  const UserReflection._({
    required this.id,
  
    required this.username,
  
    required this.firstName,
  
    required this.lastName,
  
    required this.email,
  
    required this.password,
  
    required this.phone,
  
    required this.userStatus,
  
    required this.objectWithNoDeclaredProps,
  
    required this.objectWithNoDeclaredPropsNullable,
  
    required this.anyTypeProp,
  
    required this.anyTypePropNullable,
  });

  final PropertyReflection<UndefinedWrapper<
            int
>> id;
  final PropertyReflection<UndefinedWrapper<
            String
>> username;
  final PropertyReflection<UndefinedWrapper<
            String
>> firstName;
  final PropertyReflection<UndefinedWrapper<
            String
>> lastName;
  final PropertyReflection<UndefinedWrapper<
            String
>> email;
  final PropertyReflection<UndefinedWrapper<
            String
>> password;
  final PropertyReflection<UndefinedWrapper<
            String
>> phone;
  final PropertyReflection<UndefinedWrapper<
            int
>> userStatus;
  final PropertyReflection<UndefinedWrapper<
            Map<String, Object?>
>> objectWithNoDeclaredProps;
  final PropertyReflection<UndefinedWrapper<
            Map<String, Object?>
?>> objectWithNoDeclaredPropsNullable;
  final PropertyReflection<UndefinedWrapper<Object
?>> anyTypeProp;
  final PropertyReflection<UndefinedWrapper<Object
?>> anyTypePropNullable;

  @override
  List<PropertyReflection> get members => [
    id,
username,
firstName,
lastName,
email,
password,
phone,
userStatus,
objectWithNoDeclaredProps,
objectWithNoDeclaredPropsNullable,
anyTypeProp,
anyTypePropNullable,
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
}

class UserXmlReflection {
    const UserXmlReflection();
}

