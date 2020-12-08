import 'dart:convert';

import 'package:built_value/serializer.dart';

/// Format the given parameter object into string.
String parameterToString(Serializers serializers, dynamic value) {
	if (value == null) {
		return '';
	} else if (value is String || value is num) {
		return value.toString();
	} else {
		return json.encode(serializers.serialize(value));
	}
}