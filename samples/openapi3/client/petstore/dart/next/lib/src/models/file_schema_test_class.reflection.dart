// Model reflection

part of 'file_schema_test_class.dart';


//class reflection

class FileSchemaTestClassReflection extends ClassReflection<FileSchemaTestClass> {
  static const instance = FileSchemaTestClassReflection._(
    file: PropertyReflection(
      dartName: r'file',
      nullable: false,
      required: false,
      oasName: r'file',
      oasType: r'File',
      pattern: null,
    ),
    files: PropertyReflection(
      dartName: r'files',
      nullable: false,
      required: false,
      oasName: r'files',
      oasType: r'array',
      pattern: null,
    ),
  );
  const FileSchemaTestClassReflection._({
    required this.file,
  
    required this.files,
  });

  final PropertyReflection<UndefinedWrapper<
            File
>> file;
  final PropertyReflection<UndefinedWrapper<
    List<
        
            File
>
>> files;

  @override
  List<PropertyReflection> get members => [
    file,
files,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => FileSchemaTestClass.canDeserialize(src);
  @override
  FileSchemaTestClass Function(Object? src) get deserializeFunction =>
      (src) => FileSchemaTestClass.deserialize(src);

  @override
  Object? Function(FileSchemaTestClass src) get serializeFunction =>
      (src) => src.serialize();
}

class FileSchemaTestClassXmlReflection {
    const FileSchemaTestClassXmlReflection();
}

