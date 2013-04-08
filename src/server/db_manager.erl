%%%------------------------------------
%%% @Module  : db_manager
%%% @Author  : fengzhenlin
%%% @Email   : fengzhelin@jieyou.cn
%%% @Created : 2013/3/25
%%% @Description: 管理数据库
%%%------------------------------------

-module(db_manager).
-export([start_link/0,read/1,update/1,read_ets/1,write_ets/3]).
-export([init/1,handle_call/3,handle_cast/2,terminate/2,handle_info/2,code_change/3]).
-behaviour(gen_server).

-include("debug.hrl").
-define(DB_NAME,db_connect).
%%=========================================================================
%% 接口函数
%%=========================================================================

start_link() ->
    [Ip,Name,Psw] = init:get_plain_arguments(),
    %[Ip,Name,Psw] = ["localhost","root","123"],
    {ok,Pid} = gen_server:start_link({local,?MODULE},?MODULE,[Ip,Name,Psw],[]),
    ?DEBUG("db_manager is runing",[]),
    {ok,Pid}.

%读取数据表
read(SqlStr) ->
    gen_server:call(?MODULE,{read,SqlStr}).

%写入更新数据
update(SqlStr) ->
    gen_server:call(?MODULE,{update,SqlStr}).

read_ets(id,Id) ->
    gen_server:call(?MODULE,{read_ets,id,Id});

read_ets(name,Name) ->
    gen_server:call(?MODULE,{read_ets,name,Name}).

write_ets(Id,Name,Sock) ->
    gen_server:call(?MODULE,{write_ets,Id,Name,Sock}).
%%========================================================================
%%回调函数
%%========================================================================

init([Ip,Name,Psw]) ->
    case open_db(Ip,Name,Psw) of
        {ok,_DB_Pid} -> 
            OnlineId = init_ets(),
            {ok,OnlineId};
        {error} -> {error,0}
    end.

handle_call({read,SqlStr},_From,State) ->
    {data,Res} = mysql:fetch(?DB_NAME,SqlStr),
    AllRows = mysql:get_result_rows(Res),
    {reply,AllRows,State};

handle_call({update,SqlStr},_From,State) ->
    mysql:fetch(?DB_NAME,SqlStr),
    {reply,ok,State};

handle_call({read_ets,id,Key},_From,State) ->
    Reply = ets:lookup(State,Key),
    {reply,Reply,State};

handle_call({read_ets,name,Name},_From,State) ->
    Reply = find_id_f_name(State,Name,first,[]),
    {reply,Reply,State};

handle_call({write_ets,Id,Name,Sock},_From,State) ->
    Reply = ets:insert(State,{Id,Name,Sock}),
    {reply,Reply,State}.

handle_cast(_Req,State) ->
    {noreply,State}.

handle_info(_Info,State) ->
    {noreply,State}.

terminate(_Reason,_State) ->
    ok.

code_change(_OldVsn,State,_Extra) ->
    {ok,State}.



%%=============私有函数====================
open_db(IP,Name,Psw) ->
    case mysql:start_link(?DB_NAME,IP,Name,Psw,"test") of
        {ok,Pid} ->
            ?DEBUG("opened db!",[]),
            {ok,Pid};
        Other -> 
            ?DEBUG("failed to open db,reason:~p",[Other]),
            {error}
    end.

init_ets() ->
    %id,name,socket
    ets:new(online,[set]).


%遍历online表，找出对应Name的socket
find_id_f_name(TabId,UserName,Cmd,Key1) ->
    case Cmd of
        first -> 
            case ets:first(TabId) of
                '$end_of_table' -> error;
                Value -> 
                    case ets:lookup(TabId,Value) of
                        [{Id,Name,Socket}] -> 
                           case string:equal(Name,UserName) of
                              true -> {[Id,Name,Socket]};
                              false -> find_id_f_name(TabId,UserName,next,Value)
                            end;
                        [] -> error
                    end
            end;
        next -> 
            case ets:next(TabId,Key1) of
                '$end_of_table' -> error;
                Value ->
                    case ets:lookup(TabId,Value) of
                        [{Id,Name,Socket}] -> 
                            case string:equal(Name,UserName) of
                                true -> {[Id,Name,Socket]};
                                false -> find_id_f_name(TabId,UserName,next,Value)
                            end;
                        [] -> error
                    end
            end
    end.
