// Model def

import 'package:petstore_api/_internal.dart';


part 'user.reflection.dart';
part 'user.serialization.dart';


/// UserMixin
///
/// Properties:
/// * [id] 
/// * [username] 
/// * [firstName] 
/// * [lastName] 
/// * [email] 
/// * [password] 
/// * [phone] 
/// * [userStatus] - User Status
/// * [objectWithNoDeclaredProps] - test code generation for objects Value must be a map of strings to values. It cannot be the 'null' value.
/// * [objectWithNoDeclaredPropsNullable] - test code generation for nullable objects. Value must be a map of strings to values or the 'null' value.
/// * [anyTypeProp] - test code generation for any type Here the 'type' attribute is not specified, which means the value can be anything, including the null value, string, number, boolean, array or object. See https://github.com/OAI/OpenAPI-Specification/issues/1389
/// * [anyTypePropNullable] - test code generation for any type Here the 'type' attribute is not specified, which means the value can be anything, including the null value, string, number, boolean, array or object. The 'nullable' attribute does not change the allowed values.
mixin UserMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            int

> get id;
UndefinedWrapper<
            String

> get username;
UndefinedWrapper<
            String

> get firstName;
UndefinedWrapper<
            String

> get lastName;
UndefinedWrapper<
            String

> get email;
UndefinedWrapper<
            String

> get password;
UndefinedWrapper<
            String

> get phone;
UndefinedWrapper<
            int

> get userStatus;
UndefinedWrapper<
            $FreeFormObject

> get objectWithNoDeclaredProps;
UndefinedWrapper<
            $FreeFormObject

?> get objectWithNoDeclaredPropsNullable;
UndefinedWrapper<Object

?> get anyTypeProp;
UndefinedWrapper<Object

?> get anyTypePropNullable;
  
}

/// User
///
/// Properties:
/// * [id] 
/// * [username] 
/// * [firstName] 
/// * [lastName] 
/// * [email] 
/// * [password] 
/// * [phone] 
/// * [userStatus] - User Status
/// * [objectWithNoDeclaredProps] - test code generation for objects Value must be a map of strings to values. It cannot be the 'null' value.
/// * [objectWithNoDeclaredPropsNullable] - test code generation for nullable objects. Value must be a map of strings to values or the 'null' value.
/// * [anyTypeProp] - test code generation for any type Here the 'type' attribute is not specified, which means the value can be anything, including the null value, string, number, boolean, array or object. See https://github.com/OAI/OpenAPI-Specification/issues/1389
/// * [anyTypePropNullable] - test code generation for any type Here the 'type' attribute is not specified, which means the value can be anything, including the null value, string, number, boolean, array or object. The 'nullable' attribute does not change the allowed values.
class User with
$OpenApiObjectMixin,

UserMixin {
  @override
  UndefinedWrapper<
            int

> id;
  @override
  UndefinedWrapper<
            String

> username;
  @override
  UndefinedWrapper<
            String

> firstName;
  @override
  UndefinedWrapper<
            String

> lastName;
  @override
  UndefinedWrapper<
            String

> email;
  @override
  UndefinedWrapper<
            String

> password;
  @override
  UndefinedWrapper<
            String

> phone;
  @override
  UndefinedWrapper<
            int

> userStatus;
  @override
  UndefinedWrapper<
            $FreeFormObject

> objectWithNoDeclaredProps;
  @override
  UndefinedWrapper<
            $FreeFormObject

?> objectWithNoDeclaredPropsNullable;
  @override
  UndefinedWrapper<Object

?> anyTypeProp;
  @override
  UndefinedWrapper<Object

?> anyTypePropNullable;

  AdditionalProperties<Object

?> additionalProperties;

  

  User.$all({
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
    required this.additionalProperties,
    
  });

  User({
      this.id = const UndefinedWrapper
        .undefined()
,
  this.username = const UndefinedWrapper
        .undefined()
,
  this.firstName = const UndefinedWrapper
        .undefined()
,
  this.lastName = const UndefinedWrapper
        .undefined()
,
  this.email = const UndefinedWrapper
        .undefined()
,
  this.password = const UndefinedWrapper
        .undefined()
,
  this.phone = const UndefinedWrapper
        .undefined()
,
  this.userStatus = const UndefinedWrapper
        .undefined()
,
  this.objectWithNoDeclaredProps = const UndefinedWrapper
        .undefined()
,
  this.objectWithNoDeclaredPropsNullable = const UndefinedWrapper
        .undefined()
,
  this.anyTypeProp = const UndefinedWrapper
        .undefined()
,
  this.anyTypePropNullable = const UndefinedWrapper
        .undefined()
,
    this.additionalProperties = const AdditionalProperties(),
    
  });

  static const $reflection = UserReflection.instance;
  UserReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  Map<String, dynamic> toMap() {
    return _$UserToMap(this);
  }
  factory User.fromMap(Map<String, dynamic> src) {
    return _$UserFromMap(src);
  }
  static User? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return User.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$UserCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory User.deserialize(Object? src) {
    return _$UserDeserialize(src);
  }
  static User? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return User.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$UserCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Map<String,dynamic> serialize() {
    return _$UserSerialize(this);
  }
}




