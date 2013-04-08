-module(write_pt_c).
-export([write/3]).


write(Cmdcode,Cmd_List,Data) ->
    {_RestData,ListBin,MsgLen} = write_each(Cmd_List,Data,[],0),
    Len = MsgLen + 2 + 2,
    ResultBin = [<<Len:16,Cmdcode:16>>] ++ ListBin,
    list_to_binary(ResultBin).


write_each([Type|LastL],Data,BinList,MsgLen) ->
    case Type of
        int32 ->
            [Int32 | RestData] = Data,
            NewBinList = BinList ++ [<<Int32:32>>],
            write_each(LastL,RestData,NewBinList,MsgLen+4);
        int16 ->
            [Int16 | RestData] = Data,
            NewBinList = BinList ++ [<<Int16:16>>],
            write_each(LastL,RestData,NewBinList,MsgLen+2);
        string ->
            [String | RestData ] = Data,
            StrLen = string:len(String),
            NewBinList = BinList ++ [<<StrLen:16,(list_to_binary(String))/binary>>],
            write_each(LastL,RestData,NewBinList,MsgLen+2+StrLen);
        {array,N,TypeList} ->
            io:format("array:N ~p,TypeList ~p~n",[N,TypeList]),
            TempBinList = BinList ++ [<<N:16>>],
            {RestData,ResultBinList,Len} = writing_array(N,TypeList,Data,[],0),
            NewBinList = TempBinList ++ ResultBinList,
            write_each(LastL,RestData,NewBinList,MsgLen+2+Len);
        Other ->
            io:format("~p~n",[Other]),
            {error,no_match}
    end;

write_each([],Data,BinList,MsgLen) ->
    {Data,BinList,MsgLen}.


writing_array(N,TypeList,Data,BinList,MsgLen) when N >0 ->
    {RestData,ResultBinList,Len} = write_each(TypeList,Data,[],0),
    NewBinList = ResultBinList ++ BinList,
    writing_array(N-1,TypeList,RestData,NewBinList,MsgLen+Len);

writing_array(_,_TypeList,Data,BinList,MsgLen) ->
    {Data,BinList,MsgLen}.
            
