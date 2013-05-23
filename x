#!/bin/zsh
./rebar compile && ./rebar generate && rel/skelnode/bin/skelnode console
