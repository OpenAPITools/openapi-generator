//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//


void removeNullQueryParametersExcept(
  Map<String, dynamic> queryParameters,
  Set<String> requiredParameters,
) {
  queryParameters.removeWhere((key, value) => value == null && !requiredParameters.contains(key));
}
