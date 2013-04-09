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
-export([start_link/0,mass_mailer/0]).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).

%%tcp_server监听参数
-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}, {nodelay, false}, {delay_send, true}, {send_timeout, 5000}, {keepalive, true}, {exit_on_close, true}]).
-define(TCP_PORT,3456).

-define(CMD_10205,[int32,int16,int16,int32,string,int32,string]).

%%=========================================================================
%% 接口函数
%%=========================================================================

start_link() ->
    ?DEBUG("server starting!",[]),
    case gen_server:start_link({local,?MODULE},?MODULE,[],[]) of
        {ok,Pid} ->
            ?DEBUG("server running! ~p",[Pid]),
            {ok,self()};
        Other ->
            ?DEBUG("server failed to run ~p!",[Other]),
            {error,0}
    end.


%群发邮件
mass_mailer() ->
    gen_server:call(?MODULE,{mass_mailer}).

%%=========================================================================
%% 回调函数
%%=========================================================================

init([]) ->
    process_flag(trap_exit,true),
    case gen_tcp:listen(?TCP_PORT,?TCP_OPTIONS) of
        {ok,LSock} -> 
            spawn_link(fun() -> par_connect(LSock) end);
        {error,Reason} ->
            ?DEBUG("error to listen.Reason:~p",[Reason])   
    end,
    {ok,0}.

handle_call({mass_mailer},_From,State) ->
    Title = string:strip(io:get_line("Title>"),both,$\n),
    Content = string:strip(io:get_line("Content>"),both,$\n),
    io:format("~p,~p  sfdlsdjflksjdlfj~n",[Title,Content]),
    Timestamp = util:unixtime(),
    Type = 1,   %系统邮件
    MailState = 2,  %未读

    Sql = "select id from user",
    IdList = db_manager:read(Sql),
    lists:foreach(fun([UserId])->
        %插入数据
        UpdateSql = io_lib:format("insert into mail values(0,~p,~p,~p,'~s',~p,'~s','~s')",[Type,MailState,Timestamp,"system",UserId,Title,Content]),
        db_manager:update(UpdateSql),
        %发送到用户,查询用户是否在线
        case db_manager:read_ets(id,UserId) of
            [] -> ok;
            [{_,Name,Socket}] ->
                ReadSql = io_lib:format("select id from mail where uid = ~p order by id desc limit 0,1",[UserId]),
                [[MailId]] = db_manager:read(ReadSql),
                SendData = [MailId,Type,MailState,Timestamp,"system",UserId,Title,Content],
                SendBin = write_pt:write(10205,?CMD_10205,SendData),
                gen_tcp:send(Socket,SendBin);
            _Other -> 
                ?DEBUG("mass mailer error",[])
        end
        end,
        IdList),
    Reply = ok,
    {reply,Reply,State}.

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
    Pid = spawn(fun() -> connector:start(Sock) end),
    gen_tcp:controlling_process(Sock,Pid),
    par_connect(ListenSock).
