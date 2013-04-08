%%%------------------------------------
%%% @Module  : mail_app
%%% @Author  : fengzhenlin
%%% @Email   : fengzhelin@jieyou.cn
%%% @Created : 2013/4/8
%%% @Description: 应用程序封装回调模块
%%%------------------------------------

-module(mail_app).
-behaviour(application).
-export([start/0,start/2,stop/1]).

%%=========================================================================
%% 接口函数
%%=========================================================================
start() ->
    application:start(mail).

start(_Type,_StartArgs) ->
    mail_supervisor:start().

stop(_State) ->
    ok.


