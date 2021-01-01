# Utility module for Ameba's rules.
module Ameba::AST::Util
  # Returns true if current `node` is a literal, false otherwise.
  def literal?(node)
    case node
    when Crystal::NilLiteral,
         Crystal::BoolLiteral,
         Crystal::NumberLiteral,
         Crystal::CharLiteral,
         Crystal::StringLiteral,
         Crystal::SymbolLiteral,
         Crystal::RegexLiteral,
         Crystal::ProcLiteral,
         Crystal::MacroLiteral
      true
    when Crystal::RangeLiteral
      literal?(node.from) && literal?(node.to)
    when Crystal::ArrayLiteral,
         Crystal::TupleLiteral
      node.elements.all? { |el| literal?(el) }
    when Crystal::HashLiteral
      node.entries.all? { |entry| literal?(entry.key) && literal?(entry.value) }
    when Crystal::NamedTupleLiteral
      node.entries.all? { |entry| literal?(entry.value) }
    else
      false
    end
  end

  # Returns a source code for the current node.
  # This method uses `node.location` and `node.end_location`
  # to determine and cut a piece of source of the node.
  def node_source(node, code_lines)
    loc, end_loc = node.location, node.end_location

    return unless loc && end_loc

    line, column = loc.line_number - 1, loc.column_number - 1
    end_line, end_column = end_loc.line_number - 1, end_loc.column_number - 1
    node_lines = code_lines[line..end_line]
    first_line, last_line = node_lines[0]?, node_lines[-1]?

    return if first_line.nil? || last_line.nil?
    return if first_line.size < column # compiler reports incorrection location

    node_lines[0] = first_line.sub(0...column, "")

    if line == end_line # one line
      end_column = end_column - column
      last_line = node_lines[0]
    end

    return if last_line.size < end_column + 1
    node_lines[-1] = last_line.sub(end_column + 1...last_line.size, "")

    node_lines
  end

  # Returns true if node is a flow command, false - otherwise.
  # Node represents a flow command if it is a control expression,
  # or special call node that interrupts execution (i.e. raise, exit, abort).
  def flow_command?(node, in_loop)
    case node
    when Crystal::Return
      true
    when Crystal::Break, Crystal::Next
      in_loop
    when Crystal::Call
      raise?(node) || exit?(node) || abort?(node)
    else
      false
    end
  end

  # Returns true if node is a flow expression, false if not.
  # Node represents a flow expression if it is full-filled by a flow command.
  #
  # For example, this node is a flow expression, because each branch contains
  # a flow command `return`:
  #
  # ```
  # if a > 0
  #   return :positive
  # elsif a < 0
  #   return :negative
  # else
  #   return :zero
  # end
  # ```
  #
  # This node is a not a flow expression:
  #
  # ```
  # if a > 0
  #   return :positive
  # end
  # ```
  #
  # That's because not all branches return(i.e. `else` is missing).
  #
  def flow_expression?(node, in_loop = false)
    return true if flow_command? node, in_loop

    case node
    when Crystal::If, Crystal::Unless
      flow_expressions? [node.then, node.else], in_loop
    when Crystal::BinaryOp
      flow_expression? node.left, in_loop
    when Crystal::Case
      flow_expressions? [node.whens, node.else].flatten, in_loop
    when Crystal::ExceptionHandler
      flow_expressions? [node.else || node.body, node.rescues].flatten, in_loop
    when Crystal::While, Crystal::Until
      flow_expression? node.body, in_loop
    when Crystal::Rescue, Crystal::When
      flow_expression? node.body, in_loop
    when Crystal::Expressions
      node.expressions.any? { |exp| flow_expression? exp, in_loop }
    else
      false
    end
  end

  private def flow_expressions?(nodes, in_loop)
    nodes.all? { |exp| flow_expression? exp, in_loop }
  end

  # Returns true if node represents `raise` method call.
  def raise?(node)
    node.is_a?(Crystal::Call) &&
      node.name == "raise" && node.args.size == 1 && node.obj.nil?
  end

  # Returns true if node represents `exit` method call.
  def exit?(node)
    node.is_a?(Crystal::Call) &&
      node.name == "exit" && node.args.size <= 1 && node.obj.nil?
  end

  # Returns true if node represents `abort` method call.
  def abort?(node)
    node.is_a?(Crystal::Call) &&
      node.name == "abort" && node.args.size <= 2 && node.obj.nil?
  end

  # Returns true if node represents a loop.
  def loop?(node)
    case node
    when Crystal::While, Crystal::Until
      true
    when Crystal::Call
      node.name == "loop" && node.args.size == 0 && node.obj.nil?
    else
      false
    end
  end
end
