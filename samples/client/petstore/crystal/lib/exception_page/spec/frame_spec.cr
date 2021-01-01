require "./spec_helper"

describe "Frame parsing" do
  it "returns the correct label" do
    frame = frame_for("from usr/crystal-lang/frame_spec.cr:6:7 in '->'")
    frame.label.should eq("crystal")

    frame = frame_for("from usr/crystal/frame_spec.cr:6:7 in '->'")
    frame.label.should eq("crystal")

    frame = frame_for("from lib/exception_page/spec/frame_spec.cr:6:7 in '->'")
    frame.label.should eq("exception_page")

    frame = frame_for("from lib/exception_page/frame_spec.cr:6:7 in '->'")
    frame.label.should eq("exception_page")

    frame = frame_for("from lib/frame_spec.cr:6:7 in '->'")
    frame.label.should eq("app")

    frame = frame_for("from src/frame_spec.cr:6:7 in '->'")
    frame.label.should eq("app")
  end
end

private def frame_for(backtrace_line)
  ExceptionPage::FrameGenerator.generate_frames(backtrace_line).first
end
