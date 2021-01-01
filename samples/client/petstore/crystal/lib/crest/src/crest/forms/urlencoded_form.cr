require "../form"

module Crest
  # This class lets `crest` emulate a filled-in form
  # in which a user has pressed the submit button.
  # This causes `crest` to POST data using the
  # "Content-Type" `application/x-www-form-urlencoded`.
  class UrlencodedForm(T) < Crest::Form(T)
    @content_type : String = "application/x-www-form-urlencoded"

    def generate
      @form_data = parsed_params

      self
    end

    def parsed_params
      Crest::ParamsEncoder.encode(@params)
    end
  end
end
