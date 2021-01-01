require "kemal"

ws "/" do |socket|
  socket.send "Hello from Kemal!"

  socket.on_message do |message|
    socket.send "Echo back from server #{message}"
  end
end

Kemal.run
