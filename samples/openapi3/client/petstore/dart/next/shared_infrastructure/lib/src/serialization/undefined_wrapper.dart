/// A wrapper around a type that can be undefined.
///
/// Note that the type itself can be nullable, which is irrelevant.
///
/// An undefined value means the value was not present in the source
/// when deserializing the class.
///
/// usage:
/// ```dart
/// final x = UndefinedWrapper<int>(10);
/// final y = UndefinedWrapper<int>.undefined();
/// ```
extension type const UndefinedWrapper<T>._(Object? source) {
  static const $undefinedToken = Object();

  const UndefinedWrapper.undefined() : this._($undefinedToken);
  const UndefinedWrapper(T source) : this._(source);

  bool get isUndefined => source == $undefinedToken;
  bool get isDefined => source is T;

  /// This returns the source value, or null if it's undefined.
  T? get valueOrNull => isUndefined ? null : source as T;

  /// This returns the source value, and throws an exception if it's undefined.
  T get valueRequired => isUndefined
      ? throw ArgumentError('Value is required, but it is undefined.')
      : source as T;

  bool equals(UndefinedWrapper<T> other) {
    if (other.isUndefined && isUndefined) {
      return true;
    }
    return other.source == source;
  }

  UndefinedWrapper<TOther> map<TOther>(TOther Function(T src) mapper) {
    if (isUndefined) {
      return UndefinedWrapper.undefined();
    } else {
      return UndefinedWrapper(mapper(valueRequired));
    }
  }
}


