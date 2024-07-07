// Model reflection

part of 'array_any_of.dart';

//class reflection

class ArrayAnyOfReflection extends ClassReflection<ArrayAnyOf> {
  static ArrayAnyOfReflection instanceGetter() => instance;
  static const instance = ArrayAnyOfReflection._(
    modelName: r'ArrayAnyOf',
    className: r'ArrayAnyOf',
    anyOf0Part: AnyOfReflection(
      parentReflectionGetter: instanceGetter,
    ),
    anyOf1Part: AnyOfReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<ArrayAnyOf, String>(
        parentReflectionGetter: instanceGetter,
      ),
    ),
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<ArrayAnyOf, Object?>(
        parentReflectionGetter: instanceGetter,
      ),
    ),
  );
  const ArrayAnyOfReflection._({
    required this.modelName,
    required this.className,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
    required this.anyOf0Part,
    required this.anyOf1Part,
    required this.additionalPropertiesPart,
  });

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
  List<PropertyReflection<ArrayAnyOf, dynamic>> get properties => [];

  final AdditionalPropertiesReflection<ArrayAnyOf, Object?>
      additionalPropertiesPart;

  final AnyOfReflection<ArrayAnyOf, int> anyOf0Part;

  final AnyOfReflection<ArrayAnyOf, List<String>> anyOf1Part;

  @override
  List<PartReflection<ArrayAnyOf, dynamic>> get parts => [
        ...super.parts,
        additionalPropertiesPart,
      ];
  @override
  List<AllOfReflection<ArrayAnyOf, dynamic>> get allOfs => [];

  @override
  List<OneOfReflection<ArrayAnyOf, dynamic>> get oneOfs => [];
  @override
  List<AnyOfReflection<ArrayAnyOf, dynamic>> get anyOfs => [
        anyOf0Part,
        anyOf1Part,
      ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
      (src) => ArrayAnyOf.canDeserialize(src);
  @override
  ArrayAnyOf Function(Object? src) get deserializeFunction =>
      (src) => ArrayAnyOf.deserialize(src);

  @override
  Object? Function(ArrayAnyOf src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of ArrayAnyOf.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  ArrayAnyOf example(
      {Map<String, (ClassReflection, PropertyReflection)> discriminators =
          const {}}) {
    final _reflection = this;
    if (discriminators.isEmpty)
      discriminators = _reflection.aggregatedDiscriminators;
    return ArrayAnyOf(
      additionalProperties: () {
        PartReflection? _partReflection = _reflection.additionalPropertiesPart;
        return AdditionalProperties(
            exampleMap(() => exampleNullable(() => exampleObject())));
      }(),
      anyOf0: () {
        PartReflection? _partReflection = _reflection.anyOf0Part;
        return UndefinedWrapper(exampleint());
      }(),
      anyOf1: () {
        PartReflection? _partReflection = _reflection.anyOf1Part;
        return UndefinedWrapper(exampleList(() {
          _partReflection = _partReflection?.itemsReflection;
          return exampleString();
        }));
      }(),
    );
  }
}

class ArrayAnyOfXmlReflection {
  const ArrayAnyOfXmlReflection();
}
