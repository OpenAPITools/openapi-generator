require "../spec_helper"

module Ameba
  private def it_tokenizes(str, expected)
    it "tokenizes #{str}" do
      ([] of Symbol).tap do |token_types|
        Tokenizer.new(Source.new str, normalize: false)
          .run { |token| token_types << token.type }
          .should be_true
      end.should eq expected
    end
  end

  describe Tokenizer do
    describe "#run" do
      it_tokenizes %("string"), %i(DELIMITER_START STRING DELIMITER_END EOF)
      it_tokenizes %(100), %i(NUMBER EOF)
      it_tokenizes %('a'), %i(CHAR EOF)
      it_tokenizes %([]), %i([] EOF)
      it_tokenizes %([] of String), %i([] SPACE IDENT SPACE CONST EOF)
      it_tokenizes %q("str #{3}"), %i(
        DELIMITER_START STRING INTERPOLATION_START NUMBER } DELIMITER_END EOF
      )

      it_tokenizes %(%w(1 2)),
        %i(STRING_ARRAY_START STRING STRING STRING_ARRAY_END EOF)

      it_tokenizes %(%i(one two)),
        %i(SYMBOL_ARRAY_START STRING STRING STRING_ARRAY_END EOF)

      it_tokenizes %(
          class A
            def method
              puts "hello"
            end
          end
      ), %i(
        NEWLINE SPACE IDENT SPACE CONST NEWLINE SPACE IDENT SPACE IDENT
        NEWLINE SPACE IDENT SPACE DELIMITER_START STRING DELIMITER_END
        NEWLINE SPACE IDENT NEWLINE SPACE IDENT NEWLINE SPACE EOF
      )
    end
  end
end
