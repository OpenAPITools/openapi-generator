// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'user.dart';


//class serialization

Map<String, dynamic> _$UserToMap(User instance) {
  final _reflection = UserReflection.instance;
  return <String, dynamic>{
    if (instance.id.isDefined)
    _reflection.idPart.oasName: (
            int
 v) {
      return v;
    }(instance.id.valueRequired),
    if (instance.username.isDefined)
    _reflection.usernamePart.oasName: (
            String
 v) {
      return v;
    }(instance.username.valueRequired),
    if (instance.firstName.isDefined)
    _reflection.firstNamePart.oasName: (
            String
 v) {
      return v;
    }(instance.firstName.valueRequired),
    if (instance.lastName.isDefined)
    _reflection.lastNamePart.oasName: (
            String
 v) {
      return v;
    }(instance.lastName.valueRequired),
    if (instance.email.isDefined)
    _reflection.emailPart.oasName: (
            String
 v) {
      return v;
    }(instance.email.valueRequired),
    if (instance.password.isDefined)
    _reflection.passwordPart.oasName: (
            String
 v) {
      return v;
    }(instance.password.valueRequired),
    if (instance.phone.isDefined)
    _reflection.phonePart.oasName: (
            String
 v) {
      return v;
    }(instance.phone.valueRequired),
    if (instance.userStatus.isDefined)
    _reflection.userStatusPart.oasName: (
            int
 v) {
      return v;
    }(instance.userStatus.valueRequired),
    if (instance.objectWithNoDeclaredProps.isDefined)
    _reflection.objectWithNoDeclaredPropsPart.oasName: (
            $FreeFormObject
 v) {
      return v;
    }(instance.objectWithNoDeclaredProps.valueRequired),
    if (instance.objectWithNoDeclaredPropsNullable.isDefined)
    _reflection.objectWithNoDeclaredPropsNullablePart.oasName: (
            $FreeFormObject
? v) {
      return v;
    }(instance.objectWithNoDeclaredPropsNullable.valueRequired),
    if (instance.anyTypeProp.isDefined)
    _reflection.anyTypePropPart.oasName: (Object
? v) {
      return v;
    }(instance.anyTypeProp.valueRequired),
    if (instance.anyTypePropNullable.isDefined)
    _reflection.anyTypePropNullablePart.oasName: (Object
? v) {
      return v;
    }(instance.anyTypePropNullable.valueRequired),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

User _$UserFromMap(Map<String, dynamic> src) {
  const _reflection = UserReflection.instance;
  return User.$all(
    id: src.getOrUndefinedMapped(_reflection.idPart.oasName, (v) => 
(

            
                    ( v is int ? v as int :
int.parse(v.toString())



)

)


),
username: src.getOrUndefinedMapped(_reflection.usernamePart.oasName, (v) => 
(

            
                    ( v is String ? v as String :




throwArgumentMismatch(String, v)

)

)


),
firstName: src.getOrUndefinedMapped(_reflection.firstNamePart.oasName, (v) => 
(

            
                    ( v is String ? v as String :




throwArgumentMismatch(String, v)

)

)


),
lastName: src.getOrUndefinedMapped(_reflection.lastNamePart.oasName, (v) => 
(

            
                    ( v is String ? v as String :




throwArgumentMismatch(String, v)

)

)


),
email: src.getOrUndefinedMapped(_reflection.emailPart.oasName, (v) => 
(

            
                    ( v is String ? v as String :




throwArgumentMismatch(String, v)

)

)


),
password: src.getOrUndefinedMapped(_reflection.passwordPart.oasName, (v) => 
(

            
                    ( v is String ? v as String :




throwArgumentMismatch(String, v)

)

)


),
phone: src.getOrUndefinedMapped(_reflection.phonePart.oasName, (v) => 
(

            
                    ( v is String ? v as String :




throwArgumentMismatch(String, v)

)

)


),
userStatus: src.getOrUndefinedMapped(_reflection.userStatusPart.oasName, (v) => 
(

            
                    ( v is int ? v as int :
int.parse(v.toString())



)

)


),
objectWithNoDeclaredProps: src.getOrUndefinedMapped(_reflection.objectWithNoDeclaredPropsPart.oasName, (v) => 
(

            
                    ( v is $FreeFormObject ? v as $FreeFormObject :




throwArgumentMismatch($FreeFormObject, v)

)

)


),
objectWithNoDeclaredPropsNullable: src.getOrUndefinedMapped(_reflection.objectWithNoDeclaredPropsNullablePart.oasName, (v) => 
(

            
                    (v == null ? null :  v is $FreeFormObject ? v as $FreeFormObject :




throwArgumentMismatch($FreeFormObject, v)

)

)


),
anyTypeProp: src.getOrUndefinedMapped(_reflection.anyTypePropPart.oasName, (v) => 
(
v

)
),
anyTypePropNullable: src.getOrUndefinedMapped(_reflection.anyTypePropNullablePart.oasName, (v) => 
(
v

)
),
    additionalProperties: AdditionalProperties(src.except(_reflection.knownKeys).map((key, v) => MapEntry(key, 
(
v

)
))),
    
  );
}

bool _$UserCanFromMap(Map<String, dynamic> src) {
  final _reflection = UserReflection.instance;

  if (!src.getOrUndefined(_reflection.idPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is int
     || (int.tryParse(v.toString()) != null)
    
    
    
)
),
    unDefined: () => !_reflection.idPart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.usernamePart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is String
    
    
    
    
)
),
    unDefined: () => !_reflection.usernamePart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.firstNamePart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is String
    
    
    
    
)
),
    unDefined: () => !_reflection.firstNamePart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.lastNamePart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is String
    
    
    
    
)
),
    unDefined: () => !_reflection.lastNamePart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.emailPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is String
    
    
    
    
)
),
    unDefined: () => !_reflection.emailPart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.passwordPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is String
    
    
    
    
)
),
    unDefined: () => !_reflection.passwordPart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.phonePart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is String
    
    
    
    
)
),
    unDefined: () => !_reflection.phonePart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.userStatusPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is int
     || (int.tryParse(v.toString()) != null)
    
    
    
)
),
    unDefined: () => !_reflection.userStatusPart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.objectWithNoDeclaredPropsPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    v is Map<String, dynamic>
),
    unDefined: () => !_reflection.objectWithNoDeclaredPropsPart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.objectWithNoDeclaredPropsNullablePart.oasName).split<bool>(
    defined: (v) => v == null ? true :
(

    v is Map<String, dynamic>
),
    unDefined: () => !_reflection.objectWithNoDeclaredPropsNullablePart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.anyTypePropPart.oasName).split<bool>(
    defined: (v) => v == null ? true :
(
true
),
    unDefined: () => !_reflection.anyTypePropPart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.anyTypePropNullablePart.oasName).split<bool>(
    defined: (v) => v == null ? true :
(
true
),
    unDefined: () => !_reflection.anyTypePropNullablePart.required,
)) {
    return false;
  }
  if (!src.except(_reflection.knownKeys).values.every((v) => v == null ? true :
(
true
))) {
    return false;
  }

  return true;
}

/// Deserializes a primitive Object (num, String, List, Map).
User _$UserDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$UserFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$UserCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$UserCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Map<String, dynamic> _$UserSerialize(User src) {
  Map<String, dynamic> initialResult = () {
    
    return src.toMap();
    
    
  }();
  return initialResult;
}


/*
XmlElement _$UserToXml(User instance) {
  final reflection = UserXmlReflection.instance;
  final result = XmlElement(
    XmlName(reflection.oasName, reflection.oasNamespace),
    //attributes
    [

    ],
    //elements
    [
    ],
  );
  return result;
}

User _$UserFromXml(XmlElement src) {
  final reflection = UserXmlReflection.instance;
  return User.$all(

  );
}
*/

