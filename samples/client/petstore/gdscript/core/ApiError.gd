extends Resource
class_name ApiError

# Helps finding the error in the code, among other things.
# Could be a UUID, or even a translation key, so long as it's unique.
@export var identifier := ""

# A message for humans.  May be multiline.
@export var message := ""

# One of Godot's ERR_XXXX when relevant
@export var internal_code := OK

# The HTTP response code, if any.  (usually >= 400)
@export var response_code := HTTPClient.RESPONSE_OK
