module Ameba
  # Helper module that is utilizes helpers for working with globs.
  module GlobUtils
    # Returns all files that match specified globs.
    # Globs can have wildcards or be rejected:
    #
    # ```
    # find_files_by_globs(["**/*.cr", "!lib"])
    # ```
    #
    def find_files_by_globs(globs)
      rejected = rejected_globs(globs)
      selected = globs - rejected

      expand(selected) - expand(rejected.map! { |p| p[1..-1] })
    end

    # Expands globs. Globs can point to files or even directories.
    #
    # ```
    # expand(["spec/*.cr", "src"]) # => all files in src folder + first level specs
    # ```
    #
    def expand(globs)
      globs.map do |glob|
        glob += "/**/*.cr" if File.directory?(glob)
        Dir[glob]
      end.flatten.uniq!
    end

    private def rejected_globs(globs)
      globs.select do |glob|
        glob.starts_with?('!') && !File.exists?(glob)
      end
    end
  end
end
