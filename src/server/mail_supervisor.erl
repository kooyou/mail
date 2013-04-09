%%%------------------------------------
%%% @Module  : mail_supervisor
%%% @Author  : fengzhenlin
%%% @Email   : fengzhelin@jieyou.cn
%%% @Created : 2013/4/8
%%% @Description: 邮件系统监控树
%%%------------------------------------

-module(mail_supervisor).
-behaviour(supervisor).

-export([start/0,init/1]).
%%=========================================================================
%% 接口函数
%%=========================================================================
start() ->
    {ok,Pid} = supervisor:start_link({local,?MODULE},?MODULE,[]),
    unlink(Pid),
    {ok,Pid}.

%%=========================================================================
%% 回调函数
%%=========================================================================
init([]) ->
    {ok,{
         {one_for_one,3,3600},
        [
        %数据库管理进程
        {spec_db_manager,
            {db_manager,start_link,[]},
            permanent,
            brutal_kill,
            worker,
            [chat_data]
        },
        %服务端监控进程
       {spec_mail_server,
            {server,start_link,[]},
            permanent,
            brutal_kill,
            worker,
            [server]},
        %计时器进程
       {spec_mail_timer,
            {timer_fsm,start_link,[]},
            permanent,
            brutal_kill,
            worker,
            [timer_fsm]}
        
    ]}}.
