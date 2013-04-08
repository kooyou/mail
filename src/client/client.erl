%%%------------------------------------
%%% @Module  : client
%%% @Author  : fengzhenlin
%%% @Email   : fengzhelin@jieyou.cn
%%% @Created : 2013/3/25
%%% @Description: 邮箱系统客户端
%%%------------------------------------

-module(client).
-export([start/0]).
-include("debug.hrl").
-include("protocol.hrl").
%=========================================================================
%% 接口函数
%%=========================================================================


%%=========================================================================
%% 回调函数
%%=========================================================================

-record(user,{
    id,
    name,
    passwd,
    socket,
    cmdPid
}).

%socket连接参数
-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}, {nodelay, false}, {delay_send, true}, {send_timeout, 5000}, {keepalive, true}, {exit_on_close, true}]).
-define(TCP_PORT,3456).


%协议字段列表
-define(CMD_10101,[string,string]).        %注册
-define(CMD_10001,[int32,string]).         %登录
-define(CMD_10201,[int16,string,string,string,string]).%发邮件

%建立连接
connect() ->
    case gen_tcp:connect("localhost",?TCP_PORT,?TCP_OPTIONS) of
        {ok,Socket} -> {ok,Socket};
        _Other -> 
            io:format("failed!~n")
            %?DEBUG("failed to connect server!",[])
    end.

%发送信息(简单封装gen_tcp:send)
senddata(Socket,Bin) ->
    gen_tcp:send(Socket,Bin).

%客户端启动时通过命令来确定用户的需求
%登录或者注册
start() ->
     CmdStr = io:get_line("chat>"),
     

    {Cmd,_Data} = analysis_cmd(CmdStr),
    case Cmd of
        "login" -> 
            UserId = io:get_line("UserId:"),
            UserPasswd = io:get_line("Passwd:"),
            {ID,_Rest} = string:to_integer(UserId),
            login(ID,string:sub_string(UserPasswd,1,string:len(UserPasswd)-1));
        "register" -> 
            TemName = io:get_line("UserName:"),
            TemPsw = io:get_line("Passwd:"),
            UserName = string:strip(TemName,both,$\n),
            UserPasswd = string:strip(TemPsw,both,$\n),
            %验证是否合法
            case util:is_legal(UserName,15,UserPasswd,15) of
                true -> register_c(UserName,UserPasswd);
                false -> io:format("User name or password are not legal.~n")
            end;
        Other -> 
            ?DEBUG("bad command:~p~n",[Other]),
            start()
    end.

%循环等待用户命令
wait_for_cmd(UserRecord,Pid) ->
    CmdStr = io:get_line("chat>"),
    {Cmd,Data} = analysis_cmd(CmdStr),
    case Cmd of
        "quit" -> 
            quit(UserRecord,Pid);
        "mail" ->
            mail(UserRecord,Pid),
            wait_for_cmd(UserRecord,Pid);
        _Other -> io:format("bad command!~n"),
            wait_for_cmd(UserRecord,Pid)
    end.

%退出
quit(UserRecord,RevPid) ->
    Socket = UserRecord#user.socket,
    gen_tcp:close(Socket),
    %退出接收进程
    RevPid ! stop. 


%解析输入的命令
analysis_cmd(String) ->
    %第一个空格前的字符串为命令
    Index = string:chr(String,$ ),
    StrLen = string:len(String),
    if
        (Index > 0) andalso (Index < StrLen )->
            SubTemp = string:sub_string(String,1,Index),
            SubStr = string:strip(SubTemp,both,$ ),
            LastStr1 = string:sub_string(String,Index,StrLen),
            LastStr2 = string:strip(LastStr1,both,$ ),
            LastStr = string:strip(LastStr2,both,$\n),
            {SubStr,LastStr};
        true -> { string:sub_string(String,1,string:len(String)-1),""}
    end.


%注册
register_c(UserName,Psw) ->
    case connect() of
        {ok,Socket} -> 
            SendData = [UserName,Psw],
            SendBin = write_pt_c:write(10101,?CMD_10101,SendData),
            senddata(Socket,SendBin),
            case gen_tcp:recv(Socket,?PT_HEAD_LEN) of
                {ok,Bin} ->
                    <<MsgLen:16>> = Bin,
                    %获取信息长度
                    case gen_tcp:recv(Socket,MsgLen-?PT_HEAD_LEN) of
                        {ok,DataBin} ->
                            %根据信息长度，获取信息体
                            case read_pt_c:read(<<Bin/binary,DataBin/binary>>) of
                                {ok,[_MsgLen|[_Cmd|LData]]} ->
                                    [Result,UserID] = LData,
                                    %分析注册结果
                                    case Result of
                                        ?SUCCEED ->
                                            ?DEBUG("Succeed to register,you are userid is ~p",[UserID]);
                                        ?FALSE ->
                                            ?DEBUG("failed to register!",[]);
                                        _Other ->
                                            ?DEBUG("failed to register with other problem!",[])
                                    end;
                                {error,no_match} ->
                                    ?DEBUG("no_match",[])
                            end;
                        {error,closed} ->
                            ?DEBUG("client socket closed!",[])
                    end;
                {error_closed}  ->
                    ?DEBUG("client socket closed!",[])
            end;
        error -> ?DEBUG("无法连接服务器!~n",[])
    end.

%登录到服务器
%过程：连接->登录验证
login(UserId,Psw) ->
    %创建当前用户记录
    UserRecord = #user{id=UserId,passwd=Psw,cmdPid=self()},

    %建立连接
    case connect() of
        error -> ?DEBUG("error to connect server",[]);
        {ok,Socket} -> 
    %成功建立连接
    UserId = UserRecord#user.id,
    Psw = UserRecord#user.passwd,
    Data = [UserId,Psw],
    %对请求消息进行协议封装
    SendBin = write_pt_c:write(10001,?CMD_10001,Data),
    gen_tcp:send(Socket,SendBin),
    %接收请求应答，并解包分析是否登录成功
    case gen_tcp:recv(Socket,?PT_HEAD_LEN) of
        {ok,Bin} ->
            <<MsgLen:16>> = Bin,
            case gen_tcp:recv(Socket,MsgLen-?PT_HEAD_LEN) of
                {ok,RevBin} ->
                    %解析登录应答包
                    {ok,ReadData} = read_pt_c:read(<<Bin/binary,RevBin/binary>>),
                    [_,_,Is_Succeed,_,UserName] = ReadData,
                    if
                        Is_Succeed =:= ?SUCCEED ->
                            %登录成功，向父进程发送登录成功消息
                            UserRecordNew = UserRecord#user{name=UserName,socket=Socket},
                            %连接成功，进入消息循环
                            Pid = spawn(fun() -> loop(Socket,UserRecordNew,true) end),
                            gen_tcp:controlling_process(Socket,Pid),
                            %登录成功
                            io:format("login success!~n"),
                            io:format("please type the command:~n"),
                            %进入等待命令模式
                            wait_for_cmd(UserRecordNew,Pid);

                        Is_Succeed =:= ?FALSE ->
                            ?DEBUG("False to login!~nUser name or password error",[]),
                            error;
                        true -> 
                            false
                    end;
                {error,no_match} ->
                    ?DEBUG("no_match",[])
            end;
        _Other ->
            false
    end

end. %end case

mail(UserRecord,Pid) ->
    TemName = io:get_line("RevName:"),
    TemTitle = io:get_line("title:"),
    TemContent = io:get_line("content:"),
    RevName = string:strip(TemName,both,$\n),
    Title = string:strip(TemTitle,both,$\n),
    Content = string:strip(TemContent,both,$\n),
    Data = [1,UserRecord#user.name,RevName,Title,Content],
    case write_pt:write(10201,?CMD_10201,Data) of
        {ok,Bin} ->
            gen_tcp:send(UserRecord#user.socket,Bin);
        {error,no_match} ->
            ?DEBUG("failed to send mail!",[])
    end,

    void.

%循环接收消息
loop(Socket,UserRecord,Is_auth) ->
    receive
        {tcp,Socket,Bin} ->
            {Cmdcode,MsgLen} = c_protocol_pro:get_protocol_cmd(Bin),
            RevData = c_protocol_pro:cmdcode_match(Cmdcode,MsgLen,Bin,true),
%            msg_handler(Cmdcode,MsgLen,RevData,UserRecord),
            
            loop(Socket,UserRecord,Is_auth);
        stop -> void;
        Other ->
            io:format("client rev error data:~p!~n",[Other]),
            loop(Socket,UserRecord,Is_auth)
    end.

