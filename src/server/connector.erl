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
-define(CMD_10201,[int16]).
-define(CMD_10202,[{array,[int32,int16,int16,int32,string,int32,string]}]).
-define(CMD_10203,[int16]).
-define(CMD_10204,[int32,int16,int16,int32,string,int32,string,string]).
-define(CMD_10205,[int32,int16,int16,int32,string,int32,string]).
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
                    User;
                10202 ->
                    lookup_mail(User,RestL,Bin),
                    User;
                10203 ->
                    del_mail(User,RestL,Bin),
                    User;
                10204 ->
                    browse_mail(User,RestL,Bin),
                    User
            end;
        {error,no_match} ->
            ?DEBUG("no_match~n",[]),
            User
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
            Is_login = ?SUCCEED,
            BackData = [Is_login,UserId,Name],
            ?DEBUG("User(~p ~s) login",[UserId,Name]),
            db_manager:write_ets(UserId,Name,User#user.sock);

        [] ->
            NewUser_1 = NewUser_0,
            Is_login = ?FALSE,
            BackData = [Is_login,0,""],
            ?DEBUG("User login with wrong psw!",[])
    end,
    %应答登录结果
    SendBin = write_pt:write(10001,?CMD_10001,BackData),
    sendto(User#user.sock,SendBin),

    %邮件提醒
    case Is_login of
        ?SUCCEED ->
            void;
        ?FALSE ->
            void
    end,
    NewUser_1.

mail_handle(User,Data,Bin) ->
    [Type,Sname,Uname,Title,Content] = Data,

    %查询用户是否存在
    SqlStr = io_lib:format("select id from user where name = '~s'",[Uname]),
    case db_manager:read(SqlStr) of
        [] -> void;
        [[Id]] -> 
            Time = util:unixtime(),
            %写入数据库
            AddMailSql = io_lib:format("insert into mail values(~p,~p,~p,~p,'~s',~p,'~s','~s')",
                        [0,2,2,Time,Sname,Id,Title,Content]),
            db_manager:update(AddMailSql),

             %如果用户在线，就提醒
            ReadSql = io_lib:format("select* from mail where uid = ~p order by id desc limit 0,1",[Id]),
            [ReadList] = db_manager:read(ReadSql),
            new_mail(User,ReadList,Bin)
    end,
    
    %应答
    io:format("asdfasdfsad"),
    ClientBin = write_pt:write(10201,?CMD_10201,[?SUCCEED]),
    gen_tcp:send(User#user.sock,ClientBin).


%新邮件
new_mail(_User,Data,_Bin) ->
    [Id,Type,State,Timestamp,Sname,Uid,Title,_] = Data,
    %在线消息提醒
    case Type of
        1 -> void;
        2 ->
            %检查用户是否在线
            case db_manager:read_ets(id,Uid) of
                [] -> %不在线
                    void;
                [{Uid,_,Socket}] -> %在线
                    NewData = [Id,Type,2,Timestamp,binary_to_list(Sname),Uid,binary_to_list(Title)],
                    io:format("~p~n",[NewData]),
                    SendBin = write_pt:write(10205,?CMD_10205,NewData),
                    sendto(Socket,SendBin);
                Other ->
                    Is_read = 1,
                    ?DEBUG("~p",[Other])
            end
    end,
    void.

%按条件查询邮件
lookup_mail(User,DataList,Bin) ->
    [Uid,Type] = DataList,
    case Type of
        1 ->
            SqlStr = io_lib:format("select id,type,state,timestamp,sname,uid,title from mail where uid = ~p order by state desc,timestamp desc",[Uid]);
        2 ->
            SqlStr = io_lib:format("select id,type,state,timestamp,sname,uid,title from mail where uid = ~p and state = 2 order by timestamp desc",[Uid])
    end,
    case db_manager:read(SqlStr) of
        RevMailList ->
            ListLen = util:list_length(RevMailList),
 %将二进制字符串转换成string类型
            StrList = lists:map( fun(X) ->
                                [Id,MailType,State,Timestamp,Sname,_,Title] = X,
                                [Id,MailType,State,Timestamp,binary_to_list(Sname),Uid,binary_to_list(Title)]
                                end,
                                RevMailList),
           io:format("~p",[StrList]),
           SendBin = write_pt:write(10202,?CMD_10202,[ListLen] ++ StrList),
           gen_tcp:send(User#user.sock,SendBin)
   end,
   void.

%删除邮件
del_mail(User,DataList,_Bin) ->
    [Uid,MailId] = DataList,
    SqlStr = io_lib:format("delete from mail where id = ~p and uid = '~s'",[MailId,Uid]),
    Result = db_manager:update(SqlStr),
    io:format("mysql update result: ~p~n",[Result]),
    SendBin = write_pt:write(10203,?CMD_10203,[1]),
    gen_tcp:send(User#user.sock,SendBin).


%浏览一封邮件
browse_mail(User,DataList,_Bin) ->
    [Uid,MailId] = DataList,
    SqlStr = io_lib:format("select* from mail where id = ~p",[MailId]),
    RevList = db_manager:read(SqlStr),
    [[Id,Type,State,Timestamp,Sname,Uid,Title,Content]] = RevList,
    SendBin = write_pt:write(10204,?CMD_10204,[Id,Type,State,Timestamp,binary_to_list(Sname),Uid,binary_to_list(Title),binary_to_list(Content)]),
    gen_tcp:send(User#user.sock,SendBin),

    case State of
        2 ->
            %改变为已读状态
            UpdateSql = io_lib:format("update mail set state = 1 where id = ~p",[MailId]),
            db_manager:update(UpdateSql);
        1 ->
            ok
    end.

%发送信息
sendto(Socket,Bin) ->
    gen_tcp:send(Socket,Bin).
