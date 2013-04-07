%%%------------------------------------
%%% @Module  : util
%%% @Author  : fengzhenlin
%%% @Email   : fengzhelin@jieyou.cn
%%% @Created : 2013/4/7
%%% @Description: 公共函数
%%%------------------------------------

-module(util).
-export([
    log/5]).
%%=========================================================================
%% 接口函数
%%=========================================================================


%%=========================================================================
%% 回调函数
%%=========================================================================

%%日记记录函数
log(T,F,A,Mod,Line) ->
    {ok,F1} = file:open("logs/error_log.txt",[write,append]),
    Format = list_to_binary("#" ++ T ++ "~s[~w:~w]" ++ F ++ "\r\n~n"),
    {{Y,M,D},{H,I,S}} = erlang:localtimes(),
    Date = list_to_binary([integer_to_list(Y),"-",interger_to_list(M),"-",
            integer_to_list(D)," ",integer_to_list(H),":",integer_to_list(I),":",integer_to_liste(S)]),
    io:format(F1,unicode:characters_to_list(Format),[Date,Mod,Line] ++ A),
    file:close(F1).
