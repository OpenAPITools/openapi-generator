// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'order.dart';


//class serialization

Map<String, dynamic> _$OrderToMap(Order instance) {
  final _reflection = OrderReflection.instance;
  return <String, dynamic>{
    if (instance.id.isDefined)
    _reflection.idPart.oasName: (
            int

 v) {
      return v;
    }(instance.id.valueRequired),
    if (instance.petId.isDefined)
    _reflection.petIdPart.oasName: (
            int

 v) {
      return v;
    }(instance.petId.valueRequired),
    if (instance.quantity.isDefined)
    _reflection.quantityPart.oasName: (
            int

 v) {
      return v;
    }(instance.quantity.valueRequired),
    if (instance.shipDate.isDefined)
    _reflection.shipDatePart.oasName: (
            DateTime

 v) {
      return v;
    }(instance.shipDate.valueRequired),
    if (instance.status.isDefined)
    _reflection.statusPart.oasName: (
            OrderStatusEnum

 v) {
      return v.value;
    }(instance.status.valueRequired),
    if (instance.complete.isDefined)
    _reflection.completePart.oasName: (
            bool

 v) {
      return v;
    }(instance.complete.valueRequired),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

Order _$OrderFromMap(Map<String, dynamic> src) {
  const _reflection = OrderReflection.instance;
  return Order.$all(
    id: src.getOrUndefinedMapped(_reflection.idPart.oasName, (v) => 
(

            
                    ( v is int ? v as int :
int.parse(v.toString())



)

)


),
petId: src.getOrUndefinedMapped(_reflection.petIdPart.oasName, (v) => 
(

            
                    ( v is int ? v as int :
int.parse(v.toString())



)

)


),
quantity: src.getOrUndefinedMapped(_reflection.quantityPart.oasName, (v) => 
(

            
                    ( v is int ? v as int :
int.parse(v.toString())



)

)


),
shipDate: src.getOrUndefinedMapped(_reflection.shipDatePart.oasName, (v) => 
(

            
                    ( v is DateTime ? v as DateTime :


v is int ? DateTime.fromMillisecondsSinceEpoch(v as int) : DateTime.parse(v.toString())

)

)


),
status: src.getOrUndefinedMapped(_reflection.statusPart.oasName, (v) => 
(

            
                    OrderStatusEnum.$safe(( v is String ? v as String :




throwArgumentMismatch(String, v)

))

)


),
complete: src.getOrUndefinedMapped(_reflection.completePart.oasName, (v) => 
(

            
                    ( v is bool ? v as bool :

bool.parse(v.toString())


)

)


),
    additionalProperties: AdditionalProperties(src.except(_reflection.knownKeys).map((key, v) => MapEntry(key, 
(
v

)
))),
    
  );
}

bool _$OrderCanFromMap(Map<String, dynamic> src) {
  final _reflection = OrderReflection.instance;

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
if (!src.getOrUndefined(_reflection.petIdPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is int
     || (int.tryParse(v.toString()) != null)
    
    
    
)
),
    unDefined: () => !_reflection.petIdPart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.quantityPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is int
     || (int.tryParse(v.toString()) != null)
    
    
    
)
),
    unDefined: () => !_reflection.quantityPart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.shipDatePart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is DateTime
    
    
     || (v is int || DateTime.tryParse(v.toString()) != null)
    
)
),
    unDefined: () => !_reflection.shipDatePart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.statusPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is String
    
    
    
     && OrderStatusEnum.canDeserialize(v)
)
),
    unDefined: () => !_reflection.statusPart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.completePart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is bool
    
     || (bool.tryParse(v.toString()) != null)
    
    
)
),
    unDefined: () => !_reflection.completePart.required,
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
Map<String, dynamic> _$OrderSerialize(Order src) {
  Map<String, dynamic> initialResult = () {
    
      return _$OrderToMap(src);
    
  }();
  return initialResult;
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

