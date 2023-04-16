extends Resource
class_name DemoApiResponse

# Headers sent back by the server
var headers := Dictionary()

# Raw body of this response, in String form (before deserialization)
var body := ""

# The HTTP response code, if any.  A constant like HTTPClient.RESPONSE_XXXX
var code := 0

# Deserialized body (may be pretty much any type)
var data


func _to_string() -> String:
	var s := "ApiResponse"
	if code:
		s += " %d" % code
	if body:
		s += " %s" % body
	return s
