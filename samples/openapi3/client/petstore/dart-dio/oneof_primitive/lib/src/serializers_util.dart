import 'package:one_of/any_of.dart';
import 'package:one_of/one_of.dart';

Function oneOfFactory(OneOfDynamic oneOf) {
  switch (oneOf.types.length) {
    case 1:
      return <T0>() => OneOf1<T0>(value: oneOf.value as T0);
    case 2:
      return <T0, T1>() => OneOf2<T0, T1>(
            value: oneOf.value,
            typeIndex: oneOf.typeIndex,
          );
    case 3:
      return <T0, T1, T2>() => OneOf3<T0, T1, T2>(
            value: oneOf.value,
            typeIndex: oneOf.typeIndex,
          );
    case 4:
      return <T0, T1, T2, T3>() => OneOf4<T0, T1, T2, T3>(
            value: oneOf.value,
            typeIndex: oneOf.typeIndex,
          );
    case 5:
      return <T0, T1, T2, T3, T4>() => OneOf5<T0, T1, T2, T3, T4>(
            value: oneOf.value,
            typeIndex: oneOf.typeIndex,
          );
    case 6:
      return <T0, T1, T2, T3, T4, T5>() => OneOf6<T0, T1, T2, T3, T4, T5>(
            value: oneOf.value,
            typeIndex: oneOf.typeIndex,
          );
    case 7:
      return <T0, T1, T2, T3, T4, T5, T6>() =>
          OneOf7<T0, T1, T2, T3, T4, T5, T6>(
            value: oneOf.value,
            typeIndex: oneOf.typeIndex,
          );
  }
  throw ArgumentError('Invalid number of types: ${oneOf.types.length}');
}

Function anyOfFactory(AnyOfDynamic anyOf) {
  switch (anyOf.types.length) {
    case 1:
      return <T0>() => AnyOf1<T0>(value: anyOf.values[0] as T0);
    case 2:
      return <T0, T1>() => AnyOf2<T0, T1>(
            values: anyOf.values,
          );
    case 3:
      return <T0, T1, T2>() => AnyOf3<T0, T1, T2>(
            values: anyOf.values,
          );
    case 4:
      return <T0, T1, T2, T3>() => AnyOf4<T0, T1, T2, T3>(
            values: anyOf.values,
          );
    case 5:
      return <T0, T1, T2, T3, T4>() => AnyOf5<T0, T1, T2, T3, T4>(
            values: anyOf.values,
          );
    case 6:
      return <T0, T1, T2, T3, T4, T5>() => AnyOf6<T0, T1, T2, T3, T4, T5>(
            values: anyOf.values,
          );
    case 7:
      return <T0, T1, T2, T3, T4, T5, T6>() =>
          AnyOf7<T0, T1, T2, T3, T4, T5, T6>(
            values: anyOf.values,
          );
  }
  throw ArgumentError('Invalid number of types: ${anyOf.types.length}');
}
