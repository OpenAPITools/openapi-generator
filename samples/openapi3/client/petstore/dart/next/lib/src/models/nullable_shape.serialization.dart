// Model serialization
part of 'nullable_shape.dart';


//class serialization

Map<String, dynamic> _$NullableShapeToJson(NullableShape instance) => <String, dynamic>{

};

NullableShape _$NullableShapeFromJson(Map<String, dynamic> src) {
  return NullableShape.$all(

  );
}

XmlElement _$NullableShapeToXml(NullableShape instance) {
  final reflection = NullableShapeXmlReflection.instance;
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

NullableShape _$NullableShapeFromXml(XmlElement src) {
  final reflection = NullableShapeXmlReflection.instance;
  return NullableShape.$all(

  );
}

