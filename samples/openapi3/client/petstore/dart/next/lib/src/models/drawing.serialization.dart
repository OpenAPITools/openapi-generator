// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'drawing.dart';


//class serialization

Map<String, dynamic> _$DrawingToMap(Drawing instance) {
  final _reflection = DrawingReflection.instance;
  return <String, dynamic>{
    if (instance.mainShape.isDefined)
    _reflection.mainShapePart.oasName: (
            Shape

 v) {
      return v.serialize();
    }(instance.mainShape.valueRequired),
    if (instance.shapeOrNull.isDefined)
    _reflection.shapeOrNullPart.oasName: (
            ShapeOrNull

 v) {
      return v.serialize();
    }(instance.shapeOrNull.valueRequired),
    if (instance.nullableShape.isDefined)
    _reflection.nullableShapePart.oasName: (
            NullableShape

? v) {
      return v?.serialize();
    }(instance.nullableShape.valueRequired),
    if (instance.shapes.isDefined)
    _reflection.shapesPart.oasName: (
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
  const _reflection = DrawingReflection.instance;
  return Drawing.$all(
    mainShape: src.getOrUndefinedMapped(_reflection.mainShapePart.oasName, (v) => Shape.deserialize
(

            v

)


),
shapeOrNull: src.getOrUndefinedMapped(_reflection.shapeOrNullPart.oasName, (v) => ShapeOrNull.deserialize
(

            v

)


),
nullableShape: src.getOrUndefinedMapped(_reflection.nullableShapePart.oasName, (v) => NullableShape.deserializeOrNull
(

            v

)


),
shapes: src.getOrUndefinedMapped(_reflection.shapesPart.oasName, (v) => 
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

  if (!src.getOrUndefined(_reflection.mainShapePart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            Shape.canDeserialize(v)
            
),
    unDefined: () => !_reflection.mainShapePart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.shapeOrNullPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            ShapeOrNull.canDeserialize(v)
            
),
    unDefined: () => !_reflection.shapeOrNullPart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.nullableShapePart.oasName).split<bool>(
    defined: (v) => v == null ? true :
(

    
            NullableShape.canDeserialize(v)
            
),
    unDefined: () => !_reflection.nullableShapePart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.shapesPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is List && v.every((v) => v == null ? false :
(

    
            Shape.canDeserialize(v)
            
))
),
    unDefined: () => !_reflection.shapesPart.required,
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
Map<String, dynamic> _$DrawingSerialize(Drawing src) {
  Map<String, dynamic> initialResult = () {
    
      return _$DrawingToMap(src);
    
  }();
  return initialResult;
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

