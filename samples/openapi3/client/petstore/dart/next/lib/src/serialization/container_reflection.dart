import 'package:petstore_api/_internal.dart'hide e;

import 'package:collection/collection.dart';

const $FreeFormObjectReflection =
    MapReflection(NullableReflection(ObjectReflection()));

extension SerializationReflectionExtensions<T> on SerializationReflection<T> {
  ModelReflection? getNearestModelReflection({
    bool Function(ContainerReflection reflection) shouldVisitChildOf =
        defaultShouldVisitChildOf,
  }) {
    return getNearestReflectionOfType<ModelReflection>(shouldVisitChildOf: shouldVisitChildOf);
  }
  /// searches serialization reflection tree to find a reflection that matches a given type.
  TReflection? getNearestReflectionOfType<
      TReflection extends SerializationReflection<Object?>>({
    bool Function(ContainerReflection reflection) shouldVisitChildOf =
        defaultShouldVisitChildOf,
  }) {
    return getNearestReflectionWhere(
      (reflection) => reflection is TReflection,
      shouldVisitChildOf: shouldVisitChildOf,
    ) as TReflection?;
  }

  static bool defaultShouldVisitChildOf(
      ContainerReflection containerReflection) {
    return containerReflection is! ListReflection &&
        containerReflection is! SetReflection &&
        containerReflection is! MapReflection;
  }

  SerializationReflection<Object?>? getNearestReflectionWhere(
    bool Function(SerializationReflection<Object?> reflection) predicate, {
    bool Function(ContainerReflection reflection) shouldVisitChildOf =
        defaultShouldVisitChildOf,
  }) {
    SerializationReflection<Object?> current = this;
    while (true) {
      if (predicate(current)) {
        return current;
      }
      if (current is ContainerReflection && shouldVisitChildOf(current)) {
        current = current.subReflection;
        continue;
      }
      return null;
    }
  }
}

abstract class ContainerReflection<T, TItem> extends PrimitiveReflection<T> {
  const ContainerReflection(this.subReflection);

  final SerializationReflection<TItem> subReflection;
}

class MapReflection<T> extends ContainerReflection<Map<String, T>, T> {
  const MapReflection(super.subReflection);

  @override
  Equality<Map<String, T>> get equality => MapEquality<String, T>(
        keys: PrimitiveReflection.forString.equality,
        values: subReflection.equality,
      );

  @override
  bool canDeserialize(
    Object? src, [
    SerializationContext context = const SerializationContext.json(),
  ]) {
    return context.split(
      onJson: (context) =>
          src is Map<String, Object?> &&
          src.values
              .every((v) => subReflection.canDeserializeFunction(v, context)),
      onXml: (context) {
        if (src is! XmlNode) {
          return false;
        }
        final srcNormalized = src.readNormalized();
        return srcNormalized.entries
            .where(
              (element) => element.key != null,
            )
            .every(
              (element) => subReflection.canDeserializeFunction(
                element.value,
                context.withOasNameContainer(
                  OasNameWrapper(oasName: element.key!),
                ),
              ),
            );
      },
    );
  }

  @override
  Map<String, T> deserialize(
    Object? src, [
    SerializationContext context = const SerializationContext.json(),
  ]) {
    return context.split(
      onJson: (context) => (src as Map<String, Object?>).map((key, value) =>
          MapEntry(key, subReflection.deserializeFunction(value, context))),
      onXml: (context) {
        final srcNormalized = (src as XmlNode).readNormalized();
        return Map.fromEntries(
          srcNormalized.entries
              .where(
                (element) => element.key != null,
              )
              .map(
                (e) => MapEntry<String, T>(
                  e.key!,
                  subReflection.deserializeFunction(
                    e.value,
                    context
                        .withOasNameContainer(OasNameWrapper(oasName: e.key!)),
                  ),
                ),
              )
              .where((e) => e.value != UndefinedWrapper.$undefinedToken),
        );
      },
    );
  }

  @override
  Object? serialize(
    Map<String, T> src, [
    SerializationContext context = const SerializationContext.json(),
  ]) {
    return context.split(
      onJson: (context) {
        return Map.fromEntries(
          src.entries
              .where((e) => e.value != UndefinedWrapper.$undefinedToken)
              .map(
                (e) => MapEntry(
                  e.key,
                  subReflection.serializeFunction(e.value, context),
                ),
              ),
        );
      },
      onXml: (context) {
        final wrapped = src.entries
            .where((e) => e.value != UndefinedWrapper.$undefinedToken)
            .map(
              (e) => MapEntry(
                e.key,
                subReflection.serializeFunction(
                  e.value,
                  context.withOasNameContainer(OasNameWrapper(oasName: e.key)),
                ),
              ),
            )
            .map(
              (e) => context.wrapSerializedValue(
                keyReflection: null,
                XmlName(
                  e.key,
                ),
                e.value,
              ),
            );

        return XmlDocumentFragment(wrapped);
      },
    );
  }

  @override
  Map<String, T> empty() => {};

  @override
  Map<String, T> example([ExampleContext? context]) {
    context ??= ExampleContext();
    return Map.fromEntries(
      (ListReflection(subReflection).exampleFunction(context))
          .map((e) => MapEntry(const StringReflection().example(context), e)),
    );
  }

  @override
  Map<String, T> clone(Map<String, T> src) {
    return Map.of(src);
  }
}

class ListReflection<T> extends ContainerReflection<List<T>, T> {
  const ListReflection(super.subReflection);
  @override
  Equality<List<T>> get equality => ListEquality<T>(subReflection.equality);

  @override
  bool canDeserialize(
    Object? src, [
    SerializationContext context = const SerializationContext.json(),
  ]) {
    return context.split(
      onJson: (context) =>
          src is Iterable<Object?> &&
          src.every((v) => subReflection.canDeserializeFunction(v, context)),
      onXml: (context) => (src is XmlNode &&
          src.childElements.every(
            (v) => subReflection.canDeserializeFunction(v, context),
          )),
    );
  }

  @override
  List<T> deserialize(
    Object? src, [
    SerializationContext context = const SerializationContext.json(),
  ]) {
    return context.split(
      onJson: (context) => (src as Iterable<Object?>)
          .map((value) => subReflection.deserializeFunction(value, context))
          .where((x) => x != UndefinedWrapper.$undefinedToken)
          .toList(),
      onXml: (context) {
        final node = src as XmlNode;
        final children = node.children;
        return children
            .map((v) => subReflection.deserializeFunction(v, context))
            .where((x) => x != UndefinedWrapper.$undefinedToken)
            .toList();
      },
    );
  }

  @override
  Object? serialize(
    List<T> src, [
    SerializationContext context = const SerializationContext.json(),
  ]) {
    final mapped = src
        .where((x) => x != UndefinedWrapper.$undefinedToken)
        .map((value) => subReflection.serializeFunction(value, context));
    return context.split(
      onJson: (context) {
        return mapped.toList();
      },
      onXml: (context) {
        final xml = context.xmlContainer?.xml ?? XmlReflection();
        final listName =
            xml.getQualifiedName(context.oasNameContainer?.oasName ?? 'list');
        final wrapped = mapped.map(
          (e) => context.wrapSerializedValue(
            keyReflection: xml,
            listName,
            e,
          ),
        );
        return XmlDocumentFragment(wrapped);
      },
    );
  }

  @override
  List<T> empty() => <T>[];

  @override
  List<T> example([ExampleContext? context]) {
    context ??= ExampleContext();
    return List.generate(
      context.random.nextInt(10),
      (_) => subReflection.exampleFunction(context),
    );
  }

  @override
  List<T> clone(Iterable<T> src) {
    return List.of(src);
  }
}

class SetReflection<T> extends ContainerReflection<Set<T>, T> {
  const SetReflection(super.subReflection);

  @override
  Equality<Set<T>> get equality => SetEquality(subReflection.equality);

  @override
  bool canDeserialize(
    Object? src, [
    SerializationContext context = const SerializationContext.json(),
  ]) {
    return ListReflection(subReflection).canDeserialize(src, context);
  }

  @override
  Set<T> deserialize(Object? src,
      [SerializationContext context = const SerializationContext.json()]) {
    return ListReflection(subReflection).deserialize(src, context).toSet();
  }

  @override
  Object? serialize(
    Set<T> src, [
    SerializationContext context = const SerializationContext.json(),
  ]) {
    return ListReflection(subReflection).serialize(src.toList(), context);
  }

  @override
  Set<T> empty() => {};

  @override
  Set<T> example([ExampleContext? context]) =>
      ListReflection(subReflection).example(context).toSet();

  @override
  Set<T> clone(Iterable<T> src) {
    return Set.of(src);
  }
}

class NullableReflection<T> extends ContainerReflection<T?, T?>
    implements Equality<T?> {
  const NullableReflection(super.subReflection);

  @override
  Equality<T?> get equality => this;

  bool isNull(Object? src, SerializationContext context) {
    return context.split(
      onJson: (context) => src == null,
      onXml: (context) =>
          src == null ||
          // if src is a node called xsi:nil
          (src is XmlHasName &&
              xmlNameEquals(src.name, XmlSerializationContext.nilXmlName)) ||
          (src is XmlNode &&
              (
                  // if src is a node that contains xsi:nil attribute
                  (src
                              .getAttributeOrUndefinedMapped(
                                XmlSerializationContext.nilXmlName.local,
                                (value) => bool.tryParse(value.value) ?? false,
                                namespace: XmlSerializationContext
                                    .nilXmlName.namespaceUri,
                              )
                              .valueOrNull ??
                          false) ||
                      // if src is a node that contains xsi:nil element
                      src.childElements.any(
                        (element) => xmlNameEquals(
                          element.name,
                          XmlSerializationContext.nilXmlName,
                        ),
                      ))),
    );
  }

  @override
  bool canDeserialize(
    Object? src, [
    SerializationContext context = const SerializationContext.json(),
  ]) {
    return isNull(src, context) ||
        subReflection.canDeserializeFunction(src, context);
  }

  @override
  T? deserialize(
    Object? src, [
    SerializationContext context = const SerializationContext.json(),
  ]) {
    return isNull(src, context)
        ? null
        : subReflection.deserializeFunction(src, context);
  }

  @override
  Object? serialize(
    T? src, [
    SerializationContext context = const SerializationContext.json(),
  ]) {
    return src == null ? null : subReflection.serializeFunction(src, context);
  }

  @override
  T? empty() => null;

  @override
  T? example([ExampleContext? context]) {
    context ??= ExampleContext();
    return context.random.nextBool()
        ? subReflection.exampleFunction(context)
        : null;
  }

  @override
  bool equals(T? e1, T? e2) {
    if (e1 == null || e2 == null) {
      return e1 == e2;
    }
    return subReflection.equality.equals(e1, e2);
  }

  @override
  int hash(T? e) {
    if (e == null) {
      return null.hashCode;
    }
    return subReflection.equality.hash(e);
  }

  @override
  bool isValidKey(Object? o) {
    return o == null || subReflection.equality.isValidKey(o);
  }

  @override
  T? clone(T? src) {
    return src == null ? null : subReflection.cloneFunction(src);
  }
}

class UndefinedWrapperReflection<T>
    extends ContainerReflection<UndefinedWrapper<T>, T>
    implements Equality<UndefinedWrapper<T>> {
  const UndefinedWrapperReflection(super.subReflection);

  @override
  bool canDeserialize(
    Object? src, [
    SerializationContext context = const SerializationContext.json(),
  ]) {
    return src == UndefinedWrapper.$undefinedToken ||
        subReflection.canDeserializeFunction(src, context);
  }

  @override
  UndefinedWrapper<T> deserialize(
    Object? src, [
    SerializationContext context = const SerializationContext.json(),
  ]) {
    return src == UndefinedWrapper.$undefinedToken
        ? UndefinedWrapper<T>.undefined()
        : UndefinedWrapper(subReflection.deserializeFunction(src, context));
  }

  @override
  Object? serialize(
    UndefinedWrapper<T> src, [
    SerializationContext context = const SerializationContext.json(),
  ]) {
    return src.map(
      (src) => subReflection.serializeFunction(src, context),
    );
  }

  @override
  UndefinedWrapper<T> empty() => const UndefinedWrapper.undefined();

  @override
  UndefinedWrapper<T> example([ExampleContext? context]) {
    context ??= ExampleContext();
    return context.random.nextBool()
        ? UndefinedWrapper(subReflection.exampleFunction(context))
        : const UndefinedWrapper.undefined();
  }

  @override
  Equality<UndefinedWrapper<T>> get equality => this;

  @override
  bool equals(UndefinedWrapper<T> e1, UndefinedWrapper<T> e2) {
    if (e1.isDefined && e2.isDefined) {
      return subReflection.equality.equals(e1.source as T, e2.source as T);
    }
    return e1.isDefined == e2.isDefined;
  }

  @override
  int hash(UndefinedWrapper<T> e) {
    return e.split(
      defined: (src) => subReflection.equality.hash(src),
      unDefined: () => UndefinedWrapper.$undefinedToken.hashCode,
    );
  }

  @override
  bool isValidKey(Object? o) => true;

  @override
  UndefinedWrapper<T> clone(UndefinedWrapper<T> src) {
    return src.map(
      (src) => subReflection.cloneFunction(src),
    );
  }
}

class XmlReflectionWrapper<T> extends ContainerReflection<T, T>
    with HasXmlReflection {
  const XmlReflectionWrapper(
    super.subReflection, {
    required this.xml,
  });

  final XmlReflection xml;

  @override
  bool canDeserialize(
    Object? src, [
    SerializationContext context = const SerializationContext.json(),
  ]) {
    return context.split(
      onJson: (context) => subReflection.canDeserializeFunction(src, context),
      onXml: (context) {
        if (src is MapEntry<XmlReflection, Object?>) {
          return subReflection.canDeserializeFunction(
            src.value,
            context.withXmlContainer(this),
          );
        }
        return subReflection.canDeserializeFunction(
          src,
          context.withXmlContainer(this),
        );
      },
    );
  }

  @override
  T deserialize(
    Object? src, [
    SerializationContext context = const SerializationContext.json(),
  ]) {
    return context.split(
      onJson: (context) {
        return subReflection.deserializeFunction(src, context);
      },
      onXml: (context) {
        if (src is MapEntry<XmlReflection, Object?>) {
          return subReflection.deserializeFunction(
            src.value,
            context.withXmlContainer(this),
          );
        }
        return subReflection.deserializeFunction(
          src,
          context.withXmlContainer(this),
        );
      },
    );
  }

  @override
  Object? serialize(
    T src, [
    SerializationContext context = const SerializationContext.json(),
  ]) {
    return context.split(
      onJson: (context) => subReflection.serializeFunction(src, context),
      onXml: (context) {
        return MapEntry(
          xml,
          subReflection.serializeFunction(
            src,
            context.withXmlContainer(this),
          ),
        );
      },
    );
  }

  @override
  T empty() {
    return subReflection.emptyFunction();
  }

  @override
  T example([ExampleContext? context]) {
    return subReflection.exampleFunction(context);
  }

  @override
  T clone(T src) {
    return subReflection.cloneFunction(src);
  }

  @override
  Equality<T> get equality => subReflection.equality;
}

class EnumReflection<T extends Object, TDataType extends Object>
    extends ContainerReflection<T, TDataType> {
  const EnumReflection(
    super.subReflection, {
    required this.members,
  });

  final List<EnumMemberReflection<T, TDataType>> members;

  @override
  T deserialize(Object? value,
      [SerializationContext context = const SerializationContext.json()]) {
    final deserialized = subReflection.deserializeFunction(value);
    final res = members
        .where((element) => element.oasValue == deserialized)
        .firstOrNull;
    if (res == null) {
      throw 'Invalid enum value $value';
    }
    return res.value;
  }

  @override
  bool canDeserialize(Object? value,
      [SerializationContext context = const SerializationContext.json()]) {
    if (!subReflection.canDeserializeFunction(value, context)) {
      return false;
    }
    final deserialized = subReflection.deserializeFunction(value, context);
    return members.any((element) => element.oasValue == deserialized);
  }

  @override
  Object? serialize(T value,
      [SerializationContext context = const SerializationContext.json()]) {
    return subReflection.serializeFunction(value as TDataType, context);
  }

  @override
  T empty() {
    return subReflection.emptyFunction() as T;
  }

  @override
  T example([ExampleContext? context]) {
    context ??= ExampleContext();
    final member = members.elementAt(context.random.nextInt(members.length));
    return member.value;
  }

  @override
  T clone(T src) {
    return subReflection.cloneFunction(src as TDataType) as T;
  }
}

class EnumMemberReflection<T, TDataType> {
  const EnumMemberReflection({
    required this.dartName,
    required this.oasValue,
    required this.value,
  });

  final String dartName;
  final TDataType oasValue;
  final T value;
}
