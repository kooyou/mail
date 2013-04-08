%%%------------------------------------
%%% @Module  : util
%%% @Author  : fengzhenlin
%%% @Email   : fengzhelin@jieyou.cn
%%% @Created : 2013/4/7
%%% @Description: 公共函数
%%%------------------------------------

-module(util).
-export([
    log/5,
    is_legal/4,
    unixtime/0,
    for/3]).
%%=========================================================================
%% 接口函数
%%=========================================================================


%%=========================================================================
%% 回调函数
%%=========================================================================

%%日记记录函数
log(T,F,A,Mod,Line) ->
    case file:open("../error_logs/error_log.txt",[write,append]) of
       {ok,F1} -> void;
        _Other ->
            {ok,F1} = file:open("error_logs/error_log.txt",[write,append])
    end,
    Format = list_to_binary("#" ++ T ++ "~s[~w:~w]" ++ F ++ "\r\n~n"),
    {{Y,M,D},{H,I,S}} = erlang:localtime(),
    Date = list_to_binary([integer_to_list(Y),"-",integer_to_list(M),"-",
            integer_to_list(D)," ",integer_to_list(H),":",integer_to_list(I),":",integer_to_list(S)]),
    io:format(F1,unicode:characters_to_list(Format),[Date,Mod,Line] ++ A),
    file:close(F1),

    %%%add to io
    io:format(Format,[Date,Mod,Line] ++ A).

is_legal(Name,NLen,Psw,PLen) ->
     Name_Len = string:len(Name),
    if
        Name_Len > NLen -> false;
        true -> true
    end.


%% 取得当前的unix时间戳
unixtime() ->
    {M, S, _} = erlang:now(),
    M * 1000000 + S.


%% for循环
for(Max, Max, F) ->
    F(Max);
for(I, Max, F)   ->
    F(I),
    for(I+1, Max, F).
