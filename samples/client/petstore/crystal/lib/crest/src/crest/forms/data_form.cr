require "../form"

module Crest
  # This class lets `crest` emulate a filled-in form
  # in which a user has pressed the submit button.
  # This causes `crest` to POST data using the
  # "Content-Type" `multipart/form-data according` to RFC 2388.
  # This enables uploading of binary files etc.
  class DataForm(T) < Crest::Form(T)
    def generate
      content_type_ch = Channel(String).new(1)
      io = IO::Memory.new

      HTTP::FormData.build(io) do |formdata|
        content_type_ch.send(formdata.content_type)

        # Creates an `HTTP::FormData` instance from the key-value
        # pairs of the given `params`.
        parsed_params.each do |name, value|
          add_field(formdata, name.to_s, value)
        end
      end

      @form_data = io.to_s
      @content_type = content_type_ch.receive

      self
    end

    def parsed_params
      Crest::ParamsEncoder.flatten_params(@params)
    end

    private def add_field(formdata : HTTP::FormData::Builder, name : String | Symbol, value : TextValue)
      formdata.field(name.to_s, value.to_s)
    end

    private def add_field(formdata : HTTP::FormData::Builder, name : String | Symbol, value : File)
      metadata = HTTP::FormData::FileMetadata.new(filename: value.path)
      formdata.file(name.to_s, value, metadata)
    end
  end
end
