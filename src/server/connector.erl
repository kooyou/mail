%%%------------------------------------
%%% @Module  : connector
%%% @Author  : fengzhenlin
%%% @Email   : fengzhelin@jieyou.cn
%%% @Created : 2013/3/25
%%% @Description: 管理一个客户端的连接
%%%------------------------------------

-module(connector).
-export([start/1]).
-include("protocol.hrl").
-include("debug.hrl").

-define(CMD_10101,[int16,int32]).
-define(CMD_10001,[int16,int32,string]).
-define(CMD_10201,[array,{int32,int16,int16,int32,string,int32,string,string}]).
-record(user,{
    id,
    name,
    psw,
    sock}).
%%=========================================================================
%% 接口函数
%%=========================================================================
start(Socket) ->
    User = #user{sock=Socket},
    loopRev(User).



%===========私有函数========================

%接收客户端消息
loopRev(User) ->
    Sock = User#user.sock,
    case gen_tcp:recv(Sock,?PT_HEAD_LEN) of
        {ok,Bin} ->
            <<MsgLen:16>> = Bin,
            case gen_tcp:recv(Sock,MsgLen-?PT_HEAD_LEN) of
            {ok,DataBin} ->
                io:format("test~p~n",[DataBin]),
                NewUser = dispatcher(<<MsgLen:16,DataBin/binary>>,User),
                loopRev(NewUser),
                void;
            {error,closed} ->
                ?DEBUG("client error closed,port:~p~n",[Sock])
        end;
        {error,closed} ->
            ?DEBUG("client error closed,port:~p~n",[Sock])
    end.

dispatcher(Bin,User) ->
    case read_pt:read(Bin) of
        {ok,[_MsgLen|[Cmdcode|RestL]]} ->
            case Cmdcode of
                10101 -> 
                    register_handle(User,RestL),
                    User;
                10001 -> 
                    login_handle(User,RestL);
                10201 ->
                    mail_handle(User,RestL,Bin),
                    User
            end;
        {error,no_match} ->
            ?DEBUG("no_match~n",[])
    end.

%应答注册请求
register_handle(User,Data) ->
    [Name,Psw] = Data,
    Socket = User#user.sock,
    %查询新用户名是否可用
    SqlStr = io_lib:format("select id from user where name = '~s'",[Name]),
    case db_manager:read(SqlStr) of
        [] -> 
            %获取新ID
            NewIdSql ="select max(id) from user",
            [TempId] = db_manager:read(NewIdSql),
            case TempId of
                [undefined] -> NewId = 1;
                [Num] -> 
                    NewId = Num +1
            end,
            %创建用户，写入数据库
            AddUserSql = io_lib:format("insert into user values(~p,'~s','~s')",[NewId,Name,Psw]),
            db_manager:update(AddUserSql),
            SendData = [?SUCCEED,NewId];
        _Other ->
            SendData = [?FALSE,0]
    end,
    SendBin = write_pt:write(10101,?CMD_10101,SendData),
    sendto(Socket,SendBin).

%处理登录请求
login_handle(User,Data) ->
    [UserId,StrPsw] = Data,
    NewUser_0 = User#user{id=UserId,psw=StrPsw},
    case db_manager:read_ets(id,UserId) of
        [{}] -> void;
        _Other -> ?DEBUG("User has online!",[])
    end,
    SqlStr = io_lib:format("select name from user where id=~p and psw='~s'",[UserId,StrPsw]),
    case db_manager:read(SqlStr) of
        [[Name_Bin]] ->
            %登录成功
            Name = binary_to_list(Name_Bin),
            NewUser_1 = NewUser_0#user{name=Name},
            BackData = [?SUCCEED,UserId,Name],
            ?DEBUG("User(~p ~s) login",[UserId,Name]),
            db_manager:write_ets(UserId,Name,User#user.sock),

            %查看邮件
            %未读邮件
            RevMailSql = io_lib:format("select * from mail where uid=~p and state = 2",[UserId]),
            RevMailList = db_manager:read(RevMailSql),
            lists:foreach(fun([_,Type,_,Timestamp,Sname,_,Title,Content]) ->
                        io:format("信件类型:~p~n时间:~p~n发件人名字:~p~n信件标题:~p~n信件正文:~p~n",[Type,Timestamp,Sname,Title,Content])
                          end,
                          RevMailList);

            %已发送
            %SendMailSql = io_libA:format("select * from mail where sname='~s'",[Name]),
            %SendMailList = db_manager:read(SendMailSql);
        [] ->
            NewUser_1 = NewUser_0,
            BackData = [?FALSE,0,""],
            ?DEBUG("User login with wrong psw!",[])
    end,
    SendBin = write_pt:write(10001,?CMD_10001,BackData),
    sendto(User#user.sock,SendBin),
    NewUser_1.

mail_handle(User,Data,Bin) ->
    [Type,Sname,Uname,Title,Content] = Data,
    case Type of
        1 -> void;
        2 ->
            %检查用户是否在线
            case db_manager:read_ets(name,Uname) of
                [] -> 
                    %获取id
                    SqlStr = io_lib:format("select id from user where name='~s'",[Uname]),
                    case db_manager:read(SqlStr) of
                        [[Id]] -> void;
                        Other -> 
                            ?DEBUG("~p",[Other]),
                            Id = 0
                    end,
                    Is_read = 2,
                    void;
                [{Id,_,Socket}] -> 
                    Is_read = 1,
                    sendto(Socket,Bin),
                    void;
                Other -> 
                    Id = 0,
                    Is_read = 1,
                    ?DEBUG("~p",[Other])
            end,
            if
                Id > 0 ->
                    Time = util:unixtime(),
                    %写入数据库
                    MailNum = 1,
                    MysqlData = [MailNum,0,2,Is_read,Time,Sname,Id,Title,Content],
                    write_pt:write(10201,?CMD_10201,MysqlData),
                    AddMailSql = io_lib:format("insert into mail values(~p,~p,~p,~p,'~s',~p,'~s','~s')",
                        [0,2,Is_read,Time,Sname,Id,Title,Content]),
                    db_manager:update(AddMailSql);
                true -> void
            end
    end,
    void.

%发送信息
sendto(Socket,Bin) ->
    gen_tcp:send(Socket,Bin).
