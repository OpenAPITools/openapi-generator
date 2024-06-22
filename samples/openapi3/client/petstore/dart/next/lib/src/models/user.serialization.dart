// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'user.dart';


//class serialization

Map<String, dynamic> _$UserToMap(User instance) {
  final _reflection = UserReflection.instance;
  return <String, dynamic>{
    if (instance.id.isDefined)
    _reflection.id.oasName: (
            int
 v) {
      return v;
    }(instance.id.valueRequired),
    if (instance.username.isDefined)
    _reflection.username.oasName: (
            String
 v) {
      return v;
    }(instance.username.valueRequired),
    if (instance.firstName.isDefined)
    _reflection.firstName.oasName: (
            String
 v) {
      return v;
    }(instance.firstName.valueRequired),
    if (instance.lastName.isDefined)
    _reflection.lastName.oasName: (
            String
 v) {
      return v;
    }(instance.lastName.valueRequired),
    if (instance.email.isDefined)
    _reflection.email.oasName: (
            String
 v) {
      return v;
    }(instance.email.valueRequired),
    if (instance.password.isDefined)
    _reflection.password.oasName: (
            String
 v) {
      return v;
    }(instance.password.valueRequired),
    if (instance.phone.isDefined)
    _reflection.phone.oasName: (
            String
 v) {
      return v;
    }(instance.phone.valueRequired),
    if (instance.userStatus.isDefined)
    _reflection.userStatus.oasName: (
            int
 v) {
      return v;
    }(instance.userStatus.valueRequired),
    if (instance.objectWithNoDeclaredProps.isDefined)
    _reflection.objectWithNoDeclaredProps.oasName: (
            Map<String, Object?>
 v) {
      return v;
    }(instance.objectWithNoDeclaredProps.valueRequired),
    if (instance.objectWithNoDeclaredPropsNullable.isDefined)
    _reflection.objectWithNoDeclaredPropsNullable.oasName: (
            Map<String, Object?>
? v) {
      return v;
    }(instance.objectWithNoDeclaredPropsNullable.valueRequired),
    if (instance.anyTypeProp.isDefined)
    _reflection.anyTypeProp.oasName: (Object
? v) {
      return v;
    }(instance.anyTypeProp.valueRequired),
    if (instance.anyTypePropNullable.isDefined)
    _reflection.anyTypePropNullable.oasName: (Object
? v) {
      return v;
    }(instance.anyTypePropNullable.valueRequired),
    
    
  };
}

User _$UserFromMap(Map<String, dynamic> src) {
  final _reflection = UserReflection.instance;
  return User.$all(
    id: src.getOrUndefinedMapped(_reflection.id.oasName, (v) => 
(

    
            
                    v as int
            

)


),
username: src.getOrUndefinedMapped(_reflection.username.oasName, (v) => 
(

    
            
                    v as String
            

)


),
firstName: src.getOrUndefinedMapped(_reflection.firstName.oasName, (v) => 
(

    
            
                    v as String
            

)


),
lastName: src.getOrUndefinedMapped(_reflection.lastName.oasName, (v) => 
(

    
            
                    v as String
            

)


),
email: src.getOrUndefinedMapped(_reflection.email.oasName, (v) => 
(

    
            
                    v as String
            

)


),
password: src.getOrUndefinedMapped(_reflection.password.oasName, (v) => 
(

    
            
                    v as String
            

)


),
phone: src.getOrUndefinedMapped(_reflection.phone.oasName, (v) => 
(

    
            
                    v as String
            

)


),
userStatus: src.getOrUndefinedMapped(_reflection.userStatus.oasName, (v) => 
(

    
            
                    v as int
            

)


),
objectWithNoDeclaredProps: src.getOrUndefinedMapped(_reflection.objectWithNoDeclaredProps.oasName, (v) => 
(

    v as Map<String, dynamic>

)


),
objectWithNoDeclaredPropsNullable: src.getOrUndefinedMapped(_reflection.objectWithNoDeclaredPropsNullable.oasName, (v) => 
(

    v as Map<String, dynamic>?

)


),
anyTypeProp: src.getOrUndefinedMapped(_reflection.anyTypeProp.oasName, (v) => 
(
v

)
),
anyTypePropNullable: src.getOrUndefinedMapped(_reflection.anyTypePropNullable.oasName, (v) => 
(
v

)
),
    
    
  );
}

bool _$UserCanFromMap(Map<String, dynamic> src) {
  final _reflection = UserReflection.instance;
  if (!src.getOrUndefined(_reflection.id.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is int
),
    unDefined: () => !_reflection.id.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.username.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.username.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.firstName.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.firstName.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.lastName.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.lastName.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.email.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.email.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.password.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.password.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.phone.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.phone.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.userStatus.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is int
),
    unDefined: () => !_reflection.userStatus.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.objectWithNoDeclaredProps.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    v is Map<String, dynamic>
),
    unDefined: () => !_reflection.objectWithNoDeclaredProps.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.objectWithNoDeclaredPropsNullable.oasName).split<bool>(
    defined: (v) => v == null ? true :
(

    v is Map<String, dynamic>
),
    unDefined: () => !_reflection.objectWithNoDeclaredPropsNullable.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.anyTypeProp.oasName).split<bool>(
    defined: (v) => v == null ? true :
(
true
),
    unDefined: () => !_reflection.anyTypeProp.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.anyTypePropNullable.oasName).split<bool>(
    defined: (v) => v == null ? true :
(
true
),
    unDefined: () => !_reflection.anyTypePropNullable.required,
)) {
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
Object? _$UserSerialize(User src) {
  
  return src.toMap();
  
  
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

