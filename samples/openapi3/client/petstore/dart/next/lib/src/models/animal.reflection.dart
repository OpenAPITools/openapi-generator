// Model reflection

part of 'animal.dart';

//class reflection

class AnimalReflection extends ClassReflection<Animal> {
  static AnimalReflection instanceGetter() => instance;
  static const instance = AnimalReflection._(
    modelName: r'Animal',
    className: r'Animal',
    classNamePart: PropertyReflection<Animal, String>(
      dartName: r'className',
      nullable: false,
      required: true,
      oasName: r'className',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter: instanceGetter,
      isDiscriminator: true,
      getter: _classNameGetter,
      setter: _classNameSetter,
    ),
    colorPart: PropertyReflection<Animal, UndefinedWrapper<String>>(
      dartName: r'color',
      nullable: false,
      required: false,
      oasName: r'color',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter: instanceGetter,
      isDiscriminator: false,
      getter: _colorGetter,
      setter: _colorSetter,
    ),
    discriminatorKey: r'className',
    discriminatorImplicitMappings: const {},
    discriminatorMappings: const {},
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<Animal, Object?>(
        parentReflectionGetter: instanceGetter,
      ),
    ),
  );
  const AnimalReflection._({
    required this.modelName,
    required this.className,
    required this.classNamePart,
    required this.colorPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<Animal, String> classNamePart;
  static String _classNameGetter(Animal parent) {
    return parent.className;
  }

  static void _classNameSetter(Animal parent, String value) {
    parent.className = value;
  }

  final PropertyReflection<Animal, UndefinedWrapper<String>> colorPart;
  static UndefinedWrapper<String> _colorGetter(Animal parent) {
    return parent.color;
  }

  static void _colorSetter(Animal parent, UndefinedWrapper<String> value) {
    parent.color = value;
  }

  @override
  final Map<String, ClassReflection> discriminatorMappings;
  @override
  final Map<String, ClassReflection> discriminatorImplicitMappings;
  @override
  final String? discriminatorKey;
  @override
  final String modelName;
  @override
  final String className;

  @override
  List<PropertyReflection<Animal, dynamic>> get properties => [
        classNamePart,
        colorPart,
      ];

  final AdditionalPropertiesReflection<Animal, Object?>
      additionalPropertiesPart;

  @override
  List<PartReflection<Animal, dynamic>> get parts => [
        ...super.parts,
        additionalPropertiesPart,
      ];
  @override
  List<AllOfReflection<Animal, dynamic>> get allOfs => [];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
      (src) => Animal.canDeserialize(src);
  @override
  Animal Function(Object? src) get deserializeFunction =>
      (src) => Animal.deserialize(src);

  @override
  Object? Function(Animal src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of Animal.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  Animal example(
      {Map<String, (ClassReflection, PropertyReflection)> discriminators =
          const {}}) {
    final _reflection = this;
    if (discriminators.isEmpty)
      discriminators = _reflection.aggregatedDiscriminators;
    return Animal(
      className: () {
        PartReflection? _partReflection = _reflection.classNamePart;

        final disc = discriminators[r'className'];
        if (disc != null) {
          final result = exampleDiscriminator(_partReflection, disc);
          if (result != null) {
            return result;
          }
        }

        return exampleString();
      }(),
      color: () {
        PartReflection? _partReflection = _reflection.colorPart;

        final disc = discriminators[r'color'];
        if (disc != null) {
          final result = exampleDiscriminator(_partReflection, disc);
          if (result != null) {
            return UndefinedWrapper(result);
          }
        }

        return UndefinedWrapper(exampleString());
      }(),
      additionalProperties: () {
        PartReflection? _partReflection = _reflection.additionalPropertiesPart;
        return AdditionalProperties(
            exampleMap(() => exampleNullable(() => exampleObject())));
      }(),
    );
  }
}

class AnimalXmlReflection {
  const AnimalXmlReflection();
}
