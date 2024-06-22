// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'drawing.dart';


//class serialization

Map<String, dynamic> _$DrawingToMap(Drawing instance) {
  final _reflection = DrawingReflection.instance;
  return <String, dynamic>{
    if (instance.mainShape.isDefined)
    _reflection.mainShape.oasName: (
            Shape
 v) {
      return v.serialize();
    }(instance.mainShape.valueRequired),
    if (instance.shapeOrNull.isDefined)
    _reflection.shapeOrNull.oasName: (
            ShapeOrNull
? v) {
      return v?.serialize();
    }(instance.shapeOrNull.valueRequired),
    if (instance.nullableShape.isDefined)
    _reflection.nullableShape.oasName: (
            NullableShape
? v) {
      return v?.serialize();
    }(instance.nullableShape.valueRequired),
    if (instance.shapes.isDefined)
    _reflection.shapes.oasName: (
    List<
        
            Shape
>
 v) {
      return v.map((v) => v.serialize()).toList();
    }(instance.shapes.valueRequired),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v.serialize())),
    
  };
}

Drawing _$DrawingFromMap(Map<String, dynamic> src) {
  final _reflection = DrawingReflection.instance;
  return Drawing.$all(
    mainShape: src.getOrUndefinedMapped(_reflection.mainShape.oasName, (v) => Shape.deserialize
(

    
            v


)


),
shapeOrNull: src.getOrUndefinedMapped(_reflection.shapeOrNull.oasName, (v) => ShapeOrNull.deserializeOrNull
(

    
            v


)


),
nullableShape: src.getOrUndefinedMapped(_reflection.nullableShape.oasName, (v) => NullableShape.deserializeOrNull
(

    
            v


)


),
shapes: src.getOrUndefinedMapped(_reflection.shapes.oasName, (v) => 
(

    
            
            v as List
            

)

.map((v) => Shape.deserialize
(

    
            v


)


).toList()
),
    additionalProperties: AdditionalProperties(src.except(_reflection.knownKeys).map((key, v) => MapEntry(key, Fruit.deserialize
(

    
            v


)


))),
    
  );
}

bool _$DrawingCanFromMap(Map<String, dynamic> src) {
  final _reflection = DrawingReflection.instance;
  if (!src.getOrUndefined(_reflection.mainShape.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            Shape.canDeserialize(v)
            
),
    unDefined: () => !_reflection.mainShape.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.shapeOrNull.oasName).split<bool>(
    defined: (v) => v == null ? true :
(

    
            ShapeOrNull.canDeserialize(v)
            
),
    unDefined: () => !_reflection.shapeOrNull.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.nullableShape.oasName).split<bool>(
    defined: (v) => v == null ? true :
(

    
            NullableShape.canDeserialize(v)
            
),
    unDefined: () => !_reflection.nullableShape.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.shapes.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is List && v.every((v) => v == null ? false :
(

    
            Shape.canDeserialize(v)
            
))
),
    unDefined: () => !_reflection.shapes.required,
)) {
    return false;
  }
  if (!src.except(_reflection.knownKeys).values.every((v) => v == null ? false :
(

    
            Fruit.canDeserialize(v)
            
))) {
    return false;
  }
  
  return true;
}

/// Deserializes a primitive Object (num, String, List, Map).
Drawing _$DrawingDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$DrawingFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$DrawingCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$DrawingCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Object? _$DrawingSerialize(Drawing src) {
  
  return src.toMap();
  
  
}


/*
XmlElement _$DrawingToXml(Drawing instance) {
  final reflection = DrawingXmlReflection.instance;
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

Drawing _$DrawingFromXml(XmlElement src) {
  final reflection = DrawingXmlReflection.instance;
  return Drawing.$all(

  );
}
*/

