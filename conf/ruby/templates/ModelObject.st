class $className$Model
  attr_accessor $fields:{ field | :$field.fieldDefinition.name$};separator=", "$
$fields:{ field |
  $if(field.required)$  validate_presence_of :$field.fieldDefinition.name$$endif$
}$

  # TODO: move this to base class
  def initialize(attributes = {})
    attributes.symbolize_and_underscore_keys!

    # loop over incoming attributes, only assign to valid attr_accessor props
$fields:{ field |
    if(UserModel.respond_to? :$field.fieldDefinition.name$) self.$field.fieldDefinition.name$ = attributes[:$field.fieldDefinition.name$]
}$

#  	if(UserModel.respond_to? :word) self.word = attributes[:word]
#   Zap any whitespace and bad encoding
#   attributes[:name] &&= attributes[:name].strip.squeeze(' ').force_encoding('UTF-8')
#   super(attributes)
  end
end