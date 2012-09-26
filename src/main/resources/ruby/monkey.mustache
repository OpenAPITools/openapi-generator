# module Swagger
  class Object 
  
    unless Object.method_defined? :blank?
      def blank?
        respond_to?(:empty?) ? empty? : !self
      end
    end
  
    unless Object.method_defined? :present?
      def present?
        !blank?
      end
    end

  end

  class String

    unless String.method_defined? :underscore
      def underscore
        self.gsub(/::/, '/').
        gsub(/([A-Z]+)([A-Z][a-z])/,'\1_\2').
        gsub(/([a-z\d])([A-Z])/,'\1_\2').
        tr("-", "_").
        downcase
      end
    end
  
    unless String.method_defined? :camelize
      def camelize(first_letter_in_uppercase = true)
        if first_letter_in_uppercase != :lower
          self.to_s.gsub(/\/(.?)/) { "::#{$1.upcase}" }.gsub(/(?:^|_)(.)/) { $1.upcase }
        else
          self.to_s[0].chr.downcase + camelize(self)[1..-1]
        end
      end
    end

  end

  class Hash

    unless Hash.method_defined? :stringify_keys    
      def stringify_keys
        inject({}) do |options, (key, value)|
          options[key.to_s] = value
          options
        end
      end
    end
    
    unless Hash.method_defined? :stringify_keys!
      def stringify_keys!
        self.replace(self.stringify_keys)
      end
    end

    unless Hash.method_defined? :symbolize_keys
      def symbolize_keys
        inject({}) do |options, (key, value)|
          options[(key.to_sym rescue key) || key] = value
          options
        end
      end
    end
  
    unless Hash.method_defined? :symbolize_keys!
      def symbolize_keys!
        self.replace(self.symbolize_keys)
      end
    end

    unless Hash.method_defined? :symbolize_and_underscore_keys
      def symbolize_and_underscore_keys
        inject({}) do |options, (key, value)|
          options[(key.to_s.underscore.to_sym rescue key) || key] = value
          options
        end
      end
    end

    unless Hash.method_defined? :symbolize_and_underscore_keys!
      def symbolize_and_underscore_keys!
        self.replace(self.symbolize_and_underscore_keys)
      end
    end
  
  end
# end