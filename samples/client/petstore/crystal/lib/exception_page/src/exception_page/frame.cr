# :nodoc:
struct ExceptionPage::Frame
  property index : Int32, raw_frame : Regex::MatchData

  def initialize(@raw_frame, @index)
  end

  def snippets : Array(Snippet)
    snippets = [] of Snippet
    if File.exists?(file)
      lines = File.read_lines(file)
      lines.each_with_index do |code, code_index|
        if line_is_nearby?(code_index)
          highlight = (code_index + 1 == line) ? true : false
          snippets << Snippet.new(
            line: code_index + 1,
            code: code,
            highlight: highlight
          )
        end
      end
    end
    snippets
  end

  private def line_is_nearby?(code_index : Int32)
    (code_index + 1) <= (line + 5) && (code_index + 1) >= (line - 5)
  end

  def file : String
    raw_frame[1]
  end

  def filename : String
    file.split('/').last
  end

  def line : Int32
    raw_frame[2].to_i
  end

  def args
    "#{file}:#{line}#{column_with_surrounding_method_name}"
  end

  private def column_with_surrounding_method_name
    raw_frame[3]
  end

  def label : String
    case file
    when .includes?("/crystal/"), .includes?("/crystal-lang/")
      "crystal"
    when /lib\/(?<name>[^\/]+)\/.+/
      $~["name"]
    else
      "app"
    end
  end

  def context : String
    if label == "app"
      "app"
    else
      "all"
    end
  end

  struct Snippet
    property line : Int32,
      code : String,
      highlight : Bool

    def initialize(@line, @code, @highlight)
    end
  end
end
