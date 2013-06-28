#!/usr/local/bin/luajit
require 'socket'
x = socket.udp()
x:setsockname("127.0.0.1", 9595)
d, host, port = x:receivefrom(50)
x:setpeername(host,port)
t0 = socket.gettime()
while d ~= "b" do
x:send("aaaaaaaaaaaa")
d = x:receive(50)
end
t1 = socket.gettime()
td = (t1 - t0) * 1000
print(td)
