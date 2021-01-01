module Ameba::Rule::Lint
  # A rule that disallows a rescued exception that get shadowed by a
  # less specific exception being rescued before a more specific
  # exception is rescued.
  #
  # For example, this is invalid:
  #
  # ```
  # begin
  #   do_something
  # rescue Exception
  #   handle_exception
  # rescue ArgumentError
  #   handle_argument_error_exception
  # end
  # ```
  #
  # And it has to be written as follows:
  #
  # ```
  # begin
  #   do_something
  # rescue ArgumentError
  #   handle_argument_error_exception
  # rescue Exception
  #   handle_exception
  # end
  # ```
  #
  # YAML configuration example:
  #
  # ```
  # Lint/ShadowedException:
  #   Enabled: true
  # ```
  #
  struct ShadowedException < Base
    properties do
      description "Disallows rescued exception that get shadowed"
    end

    MSG = "Exception handler has shadowed exceptions: %s"

    def test(source, node : Crystal::ExceptionHandler)
      return unless excs = node.rescues

      if (excs = shadowed excs.map(&.types)).any?
        issue_for node, MSG % excs.join(", ")
      end
    end

    private def shadowed(exceptions, exception_found = false)
      previous_exceptions = [] of String

      exceptions.reduce([] of String) do |shadowed, excs|
        excs = excs ? excs.map(&.to_s) : ["Exception"]

        if exception_found
          shadowed.concat excs
          previous_exceptions.concat excs
        else
          exception_found ||= excs.any? &.== "Exception"
          excs.each do |exc|
            if exception_found && exc != "Exception"
              shadowed << exc
            else
              shadowed << exc if previous_exceptions.any? &.== exc
            end
            previous_exceptions << exc
          end
        end

        shadowed
      end
    end
  end
end
