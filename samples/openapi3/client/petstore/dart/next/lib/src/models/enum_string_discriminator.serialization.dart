// Model serialization
part of 'enum_string_discriminator.dart';


//class serialization

Map<String, dynamic> _$EnumStringDiscriminatorToJson(EnumStringDiscriminator instance) => <String, dynamic>{

};

EnumStringDiscriminator _$EnumStringDiscriminatorFromJson(Map<String, dynamic> src) {
  return EnumStringDiscriminator.$all(

  );
}

XmlElement _$EnumStringDiscriminatorToXml(EnumStringDiscriminator instance) {
  final reflection = EnumStringDiscriminatorXmlReflection.instance;
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

EnumStringDiscriminator _$EnumStringDiscriminatorFromXml(XmlElement src) {
  final reflection = EnumStringDiscriminatorXmlReflection.instance;
  return EnumStringDiscriminator.$all(

  );
}

