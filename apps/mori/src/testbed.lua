#!/usr/local/bin/luajit
require 'socket'
require 'pack'

MORI_V1_PROTO = "b >h >h >I b A"

x = socket.udp()
x:setpeername("127.0.0.1", 9595)

print("test 1 - does formatting work?")
p = string.pack(MORI_V1_PROTO, 1, 1, 0, 0xffff, string.byte('R'), "hello world")
local ignore, ver, seq, seen, ack, cmd, arg = string.unpack(p, MORI_V1_PROTO)
print("ver: " .. ver .. " seq: " .. seq .. " seen: " .. seen .. " ack: " .. ack .. " cmd: " .. cmd .. " arg: " .. arg .. "\n")

print("test 2 - reflecting the packet")
x:send(p)
socket.sleep(0.1)
ret = x:receive()
print(ret)

print("test 3 - getting welcomed")
p = string.pack(MORI_V1_PROTO, 1, 1, 0, 0xffff, string.byte('J'), "boxyvonboxy")
x:send(p)
socket.sleep(0.1)
ret = x:receive()
print(ret)
