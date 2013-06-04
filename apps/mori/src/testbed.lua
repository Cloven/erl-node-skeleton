#!/usr/local/bin/luajit
require 'socket'
x = socket.udp()
x:setpeername("127.0.0.1", 9595)
t0 = socket.gettime()
for i = 1, 100000, 1 do
x:send("aaaaaaaaaaaa")
x:receive(50)
end
t1 = socket.gettime()
td = (t1 - t0) * 1000
print(td)
