-module(read_pt_c).
-export([read/1]).
-include("debug.hrl").

read(Bin) ->
    case Bin of
        <<MsgLen:16,Cmdcode:16,RestBin/binary>> -> 
            case read_each(routing(Cmdcode),RestBin,[]) of
                {ok,<<>>,Term} -> {ok,[MsgLen,Cmdcode] ++ Term};
                _Other -> 
                    ?DEBUG("no_match",[]),
                    {error,no_match}
            end;
        _Other -> 
            ?DEBUG("no_match",[]),
            {error,no_match}
    end.


read_each([Head|LastL],Bin,Term) ->
    case Head of
        int32 -> 
            <<Int32:32,LastBin/binary>> = Bin,
            read_each(LastL,LastBin,Term ++ [Int32]);
        int16 ->
            <<Int16:16,LastBin/binary>> = Bin,
            read_each(LastL,LastBin,Term ++ [Int16]);
        string ->
            <<Int16:16,String:Int16/binary,LastBin/binary>> = Bin,
            read_each(LastL,LastBin,Term ++ [binary_to_list(String)]);

        {array,TypeList} ->
            <<Num:16,LastBin/binary>> = Bin,
            case get_list([],LastBin,Num,TypeList) of
                {NewList,RestBin} -> read_each(LastL,RestBin,Term ++ NewList);
                error -> 
                    ?DEBUG("no_match",[]),
                    {error,no_match}
            end;
        {error,no_match} -> io:format("read_pt:read:error on_match");
        Other -> io:format("other:~p~n",[Other])
    end;

read_each([],Bin,Term) ->
    %io:format("Bin:~p~n",[Bin]),
    {ok,Bin,Term}.


    
%% 获取列表
%% AccList列表累加器，使用时初始为[]
get_list(AccList, Bin, N, TypeList) when N > 0 ->
    case Bin of
        <<Len:16, Bin2/binary>> ->
            {ok,RestBin,Term} = read_each(TypeList,Bin,[]),
            NewList = [Term | AccList],
            get_list(NewList, RestBin, N - 1, TypeList);
        _R1 ->
            ?DEBUG("error",[]),
            error
    end;
get_list(AccList, Bin, _, _) ->
    {AccList, Bin}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%根据协议码获取协议字段


routing(Cmdcode) ->
    case Cmdcode of
        10001 -> [int16,int32,string];    %登录
        10101 -> [int16,int32];   %注册
        10201 -> [int16]; %发送邮件应答
        10202 -> [{array,[int32,int16,int16,int32,string,int32,string]}]; %查询邮件
        10203 -> [int16];       %删除指定邮件
        10204 -> [int32,int16,int16,int32,string,int32,string,string]; %查询指定邮件
        10205 -> [int32,int16,int16,int32,string,int32,string];%接收邮件
        _Other -> [{error,no_match}]
    end.
