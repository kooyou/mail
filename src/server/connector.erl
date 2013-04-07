%%%------------------------------------
%%% @Module  : connector
%%% @Author  : fengzhenlin
%%% @Email   : fengzhelin@jieyou.cn
%%% @Created : 2013/3/25
%%% @Description: XXXXXX
%%%------------------------------------

-module(connector).
-export([start/1]).
-include("protocol.hrl").
-include("debug.hrl").
-record(user,{
    sock}).
%%=========================================================================
%% 接口函数
%%=========================================================================


%%=========================================================================
%% 回调函数
%%=========================================================================


start(Socket) ->
    User = #user{sock=Socket},
    loopRev(User).

%接收客户端消息
loopRev(User) ->
    Sock = User#user.sock,
    case gen_tcp:rev(Sock,?PT_HEAD_LEN) of
        {ok,Bin} ->
            io:format("~p~n",[Bin]),
            loopRev(User),
            void;
        {error,closed} ->
            ?DEBUG("client error closed,port:~p~n",[Sock])
    end.
