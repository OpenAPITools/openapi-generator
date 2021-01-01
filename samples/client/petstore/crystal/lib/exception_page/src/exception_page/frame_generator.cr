# :nodoc:
class ExceptionPage::FrameGenerator
  def self.generate_frames(message)
    generated_frames = [] of Frame
    if raw_frames = message.scan(/\s([^\s\:]+):(\d+)([^\n]+)/)
      raw_frames.each_with_index do |frame, index|
        generated_frames << Frame.new(raw_frame: frame, index: index)
      end
    end

    generated_frames
  end
end
