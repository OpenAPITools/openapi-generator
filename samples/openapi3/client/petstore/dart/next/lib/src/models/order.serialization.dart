// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'order.dart';


//class serialization

Map<String, dynamic> _$OrderToMap(Order instance) {
  final _reflection = OrderReflection.instance;
  return <String, dynamic>{
    if (instance.id.isDefined)
    _reflection.id.oasName: (
            int
 v) {
      return v;
    }(instance.id.valueRequired),
    if (instance.petId.isDefined)
    _reflection.petId.oasName: (
            int
 v) {
      return v;
    }(instance.petId.valueRequired),
    if (instance.quantity.isDefined)
    _reflection.quantity.oasName: (
            int
 v) {
      return v;
    }(instance.quantity.valueRequired),
    if (instance.shipDate.isDefined)
    _reflection.shipDate.oasName: (
            DateTime
 v) {
      return v;
    }(instance.shipDate.valueRequired),
    if (instance.status.isDefined)
    _reflection.status.oasName: (
            OrderStatusEnum
 v) {
      return v.value;
    }(instance.status.valueRequired),
    if (instance.complete.isDefined)
    _reflection.complete.oasName: (
            bool
 v) {
      return v;
    }(instance.complete.valueRequired),
    
    
  };
}

Order _$OrderFromMap(Map<String, dynamic> src) {
  final _reflection = OrderReflection.instance;
  return Order.$all(
    id: src.getOrUndefinedMapped(_reflection.id.oasName, (v) => 
(

    
            
                    v as int
            

)


),
petId: src.getOrUndefinedMapped(_reflection.petId.oasName, (v) => 
(

    
            
                    v as int
            

)


),
quantity: src.getOrUndefinedMapped(_reflection.quantity.oasName, (v) => 
(

    
            
                    v as int
            

)


),
shipDate: src.getOrUndefinedMapped(_reflection.shipDate.oasName, (v) => 
(

    
            
                    v as DateTime
            

)


),
status: src.getOrUndefinedMapped(_reflection.status.oasName, (v) => 
(

    
            
                    OrderStatusEnum.$safe(v as String)
            

)


),
complete: src.getOrUndefinedMapped(_reflection.complete.oasName, (v) => 
(

    
            
                    v as bool
            

)


),
    
    
  );
}

bool _$OrderCanFromMap(Map<String, dynamic> src) {
  final _reflection = OrderReflection.instance;
  if (!src.getOrUndefined(_reflection.id.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is int
),
    unDefined: () => !_reflection.id.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.petId.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is int
),
    unDefined: () => !_reflection.petId.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.quantity.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is int
),
    unDefined: () => !_reflection.quantity.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.shipDate.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is DateTime
),
    unDefined: () => !_reflection.shipDate.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.status.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.status.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.complete.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is bool
),
    unDefined: () => !_reflection.complete.required,
)) {
    return false;
  }
  
  return true;
}

/// Deserializes a primitive Object (num, String, List, Map).
Order _$OrderDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$OrderFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$OrderCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$OrderCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Object? _$OrderSerialize(Order src) {
  
  return src.toMap();
  
  
}


/*
XmlElement _$OrderToXml(Order instance) {
  final reflection = OrderXmlReflection.instance;
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

Order _$OrderFromXml(XmlElement src) {
  final reflection = OrderXmlReflection.instance;
  return Order.$all(

  );
}
*/

