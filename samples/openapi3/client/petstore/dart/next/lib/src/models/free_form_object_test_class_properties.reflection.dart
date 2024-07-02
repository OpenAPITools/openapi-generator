// Model reflection

part of 'free_form_object_test_class_properties.dart';


//class reflection

class FreeFormObjectTestClassPropertiesReflection extends ClassReflection<FreeFormObjectTestClassProperties> {
  static const instance = FreeFormObjectTestClassPropertiesReflection._(
  );
  const FreeFormObjectTestClassPropertiesReflection._();


  @override
  List<PropertyReflection> get members => [
      ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => FreeFormObjectTestClassProperties.canDeserialize(src);
  @override
  FreeFormObjectTestClassProperties Function(Object? src) get deserializeFunction =>
      (src) => FreeFormObjectTestClassProperties.deserialize(src);

  @override
  Object? Function(FreeFormObjectTestClassProperties src) get serializeFunction =>
      (src) => src.serialize();
}

class FreeFormObjectTestClassPropertiesXmlReflection {
    const FreeFormObjectTestClassPropertiesXmlReflection();
}

