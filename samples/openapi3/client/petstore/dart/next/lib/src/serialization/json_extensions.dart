import 'package:shared_infrastructure/shared_infrastructure.dart';

extension UndefinedWrapperMapExtensions<TKey, TValue> on Map<TKey, TValue> {
  UndefinedWrapper<TValue?> getOrUndefined(
    TKey key,
  ) {
    if (!containsKey(key)) {
      return UndefinedWrapper.undefined();
    } else {
      return UndefinedWrapper(this[key]);
    }
  }

  UndefinedWrapper<T> getOrUndefinedMapped<T>(
    TKey key,
    T Function(TValue? src) mapper,
  ) {
    return getOrUndefined(key).map(mapper);
  }

  T getRequiredMapped<T>(
    TKey key,
    T Function(TValue? src) mapper,
  ) {
    if (!containsKey(key)) {
      throw ArgumentError(
        'key ($key) is not present in the source Map, but it is required.',
        'key',
      );
    } else {
      return mapper(this[key]);
    }
  }
}