require "./spec_helper"

describe ExceptionPage do
  it "allows debugging the exception page" do
    flow = ErrorDebuggingFlow.new

    flow.view_error_page
    flow.should_have_information_for_debugging
    flow.show_all_frames
    flow.should_be_able_to_view_other_frames
  end
end

class ErrorDebuggingFlow < LuckyFlow
  def view_error_page
    visit "/"
  end

  def should_have_information_for_debugging
    el("@exception-title", text: "Something went very wrong").should be_on_page
    el("@code-frames", text: "test_server.cr").should be_on_page
    click("@see-raw-error-message")
    el("@raw-error-message").should be_on_page
  end

  def show_all_frames
    el("@show-all-frames").click
  end

  def should_be_able_to_view_other_frames
    el("@code-frame-file", "request_processor.cr").click
    el("@code-frame-summary", text: "request_processor.cr").should be_on_page
  end
end
