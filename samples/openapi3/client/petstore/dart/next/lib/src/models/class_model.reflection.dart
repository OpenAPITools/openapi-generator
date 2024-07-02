// Model reflection

part of 'class_model.dart';


//class reflection

class ClassModelReflection extends ClassReflection<ClassModel> {
  static const instance = ClassModelReflection._(
    propertyClass: PropertyReflection(
      dartName: r'propertyClass',
      nullable: false,
      required: false,
      oasName: r'_class',
      oasType: r'string',
      pattern: null,
    ),
  );
  const ClassModelReflection._({
    required this.propertyClass,
  });

  final PropertyReflection<UndefinedWrapper<
            String
>> propertyClass;

  @override
  List<PropertyReflection> get members => [
    propertyClass,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => ClassModel.canDeserialize(src);
  @override
  ClassModel Function(Object? src) get deserializeFunction =>
      (src) => ClassModel.deserialize(src);

  @override
  Object? Function(ClassModel src) get serializeFunction =>
      (src) => src.serialize();
}

class ClassModelXmlReflection {
    const ClassModelXmlReflection();
}

