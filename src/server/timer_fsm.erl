%%%------------------------------------
%%% @Module  : timer_fsm
%%% @Author  : fengzhenlin
%%% @Email   : fengzhelin@jieyou.cn
%%% @Created : 2013/3/25
%%% @Description: 计时器，负责定时删除邮件
%%%------------------------------------

-module(timer_fsm).
-behaviour(gen_fsm).
-export([
        start_link/0,
        stop/0
        ]).

-export([init/1,waiting/2,handle_event/3,handle_sync_event/4,handle_info/3,terminate/3,code_change/4]).

-include("debug.hrl").
-define(WAITINGTIME,5*1000).
-define(DELMAILTIME,7*24*60*60).%一周秒数
%-define(WAITINGTIME,12*60*60*1000).
%%=========================================================================
%% 接口函数
%%=========================================================================
start_link() ->
    ?DEBUG("starting [~w]...",[?MODULE]),

    gen_fsm:start_link({local,?MODULE},?MODULE,[],[]).

stop() ->
    ok.

%%=========================================================================
%% 回调函数
%%=========================================================================

init([]) ->
    {ok,waiting,0,?WAITINGTIME}.

handle_event(_Event,StateName,State) ->
    {nex_state,StateName,State}.

handle_sync_event(_Event,_From,StateName,State) ->
    Reply = ok,
    {reply,Reply,StateName,State}.

handle_info(_Info,StateName,State) ->
    {next_state,StateName,State}.

terminate(_Reason,_StateName,_State) ->
    ok.

code_change(_OldVsn,StateName,State,_Extra) ->
    {ok,StateName,State}.


%%=======================私有函数==================================

waiting(timeout,State) ->
    time_to_del_mail(),
    {next_state,waiting,0,?WAITINGTIME}.

time_to_del_mail() ->
    Sql = "select * from mail",
    case db_manager:read(Sql) of
        [] ->
            ok;
        ResultList ->
            lists:foreach(fun(Item) ->
                        [_,_,_,Timestamp,_,_,_,_] = Item,
                        CurrentTimestamp = util:unixtime(),
                        TimeSpan = CurrentTimestamp - Timestamp,
                        case TimeSpan >=?DELMAILTIME of %一周
                            true ->
                                del_one_mail(Item);
                            false ->
                                ok
                        end
                    end,
                    ResultList)
    end.


del_one_mail(Mail) ->
    [Id,_,_,_,_,_,_,_] = Mail,
    Sql = io_lib:format("delete from mail where id = ~p",[Id]),
    db_manager:update(Sql).
