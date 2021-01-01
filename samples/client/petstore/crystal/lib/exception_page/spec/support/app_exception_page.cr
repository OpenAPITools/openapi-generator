class MyApp::ExceptionPage < ExceptionPage
  def styles : Styles
    Styles.new(accent: "purple")
  end

  def stack_trace_heading_html
    <<-HTML
    <a href="#" onclick="sayHi()">Say hi</a>
    HTML
  end

  def extra_javascript
    <<-JAVASCRIPT
    window.sayHi = function() {
      alert("Say Hi!");
    }
    JAVASCRIPT
  end
end
