import 'package:petstore_api/_internal.dart';

abstract class ClassReflection<T> {
  const ClassReflection();
  String get modelName;
  String get className;

  String? get discriminatorKey;
  Map<String, ClassReflection> get discriminatorMappings;
  Map<String, ClassReflection> get discriminatorImplicitMappings;

  List<OneOfReflection<T, dynamic>> get oneOfs => [];
  List<AnyOfReflection<T, dynamic>> get anyOfs => [];
  List<AllOfReflection<T, dynamic>> get allOfs => [];

  ClassReflection? tryGetDiscriminatorModel(String? discriminatorValue) {
    if (discriminatorValue == null) {
      return null;
    }
    return discriminatorMappings[discriminatorValue] ??
          discriminatorImplicitMappings[discriminatorValue];
  }

  static final _cachedAggregatedDiscriminators =
      Expando<Map<String, (ClassReflection, PropertyReflection)>>();

  Map<String, (ClassReflection, PropertyReflection)>
      get aggregatedDiscriminators => _cachedAggregatedDiscriminators[this] ??=
          Map.fromEntries(_aggregatedDiscriminators(this)
              .map((e) => MapEntry(e.$2.oasName, e)));

  static Iterable<(ClassReflection, PropertyReflection)>
      _aggregatedDiscriminators(
    ClassReflection reflection,
  ) sync* {
    if (reflection.discriminatorKey != null) {
      yield* reflection.properties
          .where((x) => x.oasName == reflection.discriminatorKey)
          .map((e) => (reflection, e));
    }
    yield* reflection.oneOfs
        .map((e) => e.classReflection)
        .followedBy(reflection.anyOfs.map((e) => e.classReflection))
        .nonNulls
        .expand(_aggregatedDiscriminators);
  }

  List<PartReflection<T, dynamic>> get parts => [
    ...oneOfs,
    ...anyOfs,
    ...allOfs,
    ...properties,
  ];
  List<PropertyReflection<T, dynamic>> get properties;
  Set<String> get knownKeys => properties.map((e) => e.oasName).toSet();

  T Function(Object? src) get deserializeFunction;
  bool Function(Object? src) get canDeserializeFunction;
  Object? Function(T src) get serializeFunction;
}

abstract class PartReflection<TParent, T> {
  const PartReflection({
    this.classReflection,
    this.parentReflectionGetter,
    this.itemsReflection,
  });

  final ClassReflection? classReflection;
  final ClassReflection<TParent> Function()? parentReflectionGetter;
  /// If this part represents a Map or a List, pass [itemsReflection] to identify the items type.
  final ItemsReflection<TParent, dynamic>? itemsReflection;
}

class ItemsReflection<TParent, T> extends PartReflection<TParent, T> {
  const ItemsReflection({
    super.classReflection,
    super.parentReflectionGetter,
    super.itemsReflection,
  });
}

class PropertyReflection<TParent, T> extends PartReflection<TParent, T> {
  const PropertyReflection({
    required this.dartName,
    required this.oasName,
    required this.oasType,
    required this.required,
    required this.nullable,
    this.$default,
    this.pattern,
    super.classReflection,
    super.parentReflectionGetter,
    super.itemsReflection,
    required this.getter,
    required this.setter,
    required this.isDiscriminator,
  });

  final String dartName;
  final String oasName;
  final String oasType;
  final bool required;
  final bool nullable;
  final Object? $default;
  final String? pattern;


  final T Function(TParent parent) getter;
  final void Function(TParent parent, T value) setter;
  final bool isDiscriminator;
}

class AdditionalPropertiesReflection<TParent, T> extends PartReflection<TParent, AdditionalProperties<T>> {
  const AdditionalPropertiesReflection({
    super.classReflection,
    super.parentReflectionGetter,
    super.itemsReflection,
  });
}

class OneOfReflection<TParent, T> extends PartReflection<TParent, T> {
  const OneOfReflection({
    super.classReflection,
    super.parentReflectionGetter,
    super.itemsReflection,
  });
}
class AnyOfReflection<TParent, T> extends PartReflection<TParent, T> {
  const AnyOfReflection({
    super.classReflection,
    super.parentReflectionGetter,
    super.itemsReflection,
  });
}

class AllOfReflection<TParent, T> extends PartReflection<TParent, T> {
  const AllOfReflection({
    super.classReflection,
    super.parentReflectionGetter,
  });
}

// abstract class EnumReflection<T extends Enum> {
//   const EnumReflection();
//   Type get type => T;
//   List<EnumMemberReflection> get members;
// }

class EnumMemberReflection {
  const EnumMemberReflection({
    required this.dartName,
    required this.oasValue,
  });
  final String dartName;
  final Object? oasValue;
}
