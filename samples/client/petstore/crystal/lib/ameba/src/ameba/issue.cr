module Ameba
  # Represents an issue reported by Ameba.
  record Issue,
    # A rule that triggers this issue.
    rule : Rule::Base,

    # Location of the issue.
    location : Crystal::Location?,

    # End location of the issue.
    end_location : Crystal::Location?,

    # Issue message.
    message : String,

    # Issue status.
    status : Symbol? do
    def disabled?
      status == :disabled
    end

    def syntax?
      rule.is_a?(Rule::Lint::Syntax)
    end
  end
end
