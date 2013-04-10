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
    string_is_legal/2,
    unixtime/0,
    timestamp_to_localtime/1,
    for/3,
    list_length/1]).
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

%检验一般的用户名和密码是否合法
is_legal(Name,NLen,Psw,PLen) ->
     Name_Len = string:len(Name),
    if
        Name_Len > NLen -> false;
        true -> true
    end.

%检验字符串是否合法
string_is_legal(String,NLen) ->
    StrLen = string:len(String),
    if
        StrLen > NLen ->
            false;
        true ->
            true
    end.


%% 取得当前的unix时间戳
unixtime() ->
    {M, S, _} = erlang:now(),
    M * 1000000 + S.

%%unix时间戳->{{Y,M,D},{H,N,S}}
timestamp_to_localtime(Timestamp) ->
    MM = Timestamp div 1000000,
    SS = Timestamp - MM*1000000,
    MI = Timestamp - MM*1000000 - SS*1000,
    calendar:now_to_local_time({MM,SS,MI}).


%% for循环
for(Max, Max, F) ->
    F(Max);
for(I, Max, F)   ->
    F(I),
    for(I+1, Max, F).


%获取列表长度    
list_length([]) ->
     0; 

list_length([First | Rest]) ->
     1 + list_length(Rest).
