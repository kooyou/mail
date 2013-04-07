%%%------------------------------------
%%% @Module  : server
%%% @Author  : fengzhenlin
%%% @Email   : fengzhelin@jieyou.cn
%%% @Created : 2013/4/7
%%% @Description: 邮件系统服务端管理
%%%------------------------------------

-module(server).
-behaviour(gen_server).
-include("debug.hrl").
-export([start/0]).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).

%%tcp_server监听参数
-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}, {nodelay, false}, {delay_send, true}, {send_timeout, 5000}, {keepalive, true}, {exit_on_close, true}]).
-define(TCP_PORT,3456).

%%=========================================================================
%% 接口函数
%%=========================================================================


%%=========================================================================
%% 回调函数
%%=========================================================================

start() ->
    gen_server:start_link({local,?MODULE},?MODULE,[],[]).

init([]) ->
    process_flag(trap_exit,true),
    case get_tcp:listen(?TCP_PORT,?TCP_OPTIONS) of
        {ok,LSock} -> 
            spawn_link(fun() -> par_connect(LSock) end);
        {error,Reason} ->
            ?DEBUG("error to listen.Reason:~p",[Reason])   
    end,
    {ok,0}.

handle_call(_Request,_From,State) ->
    Reply = ok,
    {repley,Reply,State}.

handle_cast(_Msg,State) ->
    {noreply,State}.

handle_info(_Info,State) ->
    {noreply,State}.

terminate(_Reason,_State) ->
    ok.

code_change(_OldVsn,State,_Extra) ->
    {ok,State}.


%%===============================================================================
%%私有函数
%%===============================================================================

%并行连接
par_connect(ListenSock) ->
    {ok,Sock} = gen_tcp:accept(ListenSock),
    {ok,Pid} = spawn(fun() -> connector:start(Sock) end),
    gen_tcp:controlling_process(Sock,Pid),
    par_connect(ListenSock).
