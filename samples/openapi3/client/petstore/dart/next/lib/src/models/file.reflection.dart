// Model reflection

part of 'file.dart';


//class reflection

class FileReflection extends ClassReflection<File> {
  static const instance = FileReflection._(
    sourceURI: PropertyReflection(
      dartName: r'sourceURI',
      nullable: false,
      required: false,
      oasName: r'sourceURI',
      oasType: r'string',
      pattern: null,
    ),
  );
  const FileReflection._({
    required this.sourceURI,
  });

  final PropertyReflection<UndefinedWrapper<
            String
>> sourceURI;

  @override
  List<PropertyReflection> get members => [
    sourceURI,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => File.canDeserialize(src);
  @override
  File Function(Object? src) get deserializeFunction =>
      (src) => File.deserialize(src);

  @override
  Object? Function(File src) get serializeFunction =>
      (src) => src.serialize();
}

class FileXmlReflection {
    const FileXmlReflection();
}

