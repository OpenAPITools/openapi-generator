//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: true,
  explicitToJson: true,
)
class ModelClient {
  /// Returns a new [ModelClient] instance.
  ModelClient({
    this.client,
  });

  @JsonKey(
    nullable: false,
    name: r'client',
    required: false,
  )
  String client;

  @override
  bool operator ==(Object other) => identical(this, other) || other is ModelClient &&
     other.client == client;

  @override
  int get hashCode =>
    (client == null ? 0 : client.hashCode);

  factory ModelClient.fromJson(Map<String, dynamic> json) => _$ModelClientFromJson(json);

  Map<String, dynamic> toJson() => _$ModelClientToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

