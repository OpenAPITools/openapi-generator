module Crest
  abstract class Form(T)
    @form_data : String = ""
    @content_type : String = ""

    getter params, form_data, content_type

    def self.generate(params : Hash)
      new(params).generate
    end

    def initialize(@params : T)
    end

    abstract def generate
  end
end
