import 'package:petstore_api/_internal.dart'hide e;
import 'package:collection/collection.dart';

const $FreeFormObjectReflection =
    MapReflection(NullableReflection(ObjectReflection()));

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