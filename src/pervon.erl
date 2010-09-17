%% Copyright (c) 2010, Michael Santos <michael.santos@gmail.com>
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met:
%%
%% Redistributions of source code must retain the above copyright
%% notice, this list of conditions and the following disclaimer.
%%
%% Redistributions in binary form must reproduce the above copyright
%% notice, this list of conditions and the following disclaimer in the
%% documentation and/or other materials provided with the distribution.
%%
%% Neither the name of the author nor the names of its contributors
%% may be used to endorse or promote products derived from this software
%% without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
%% FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
%% COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
%% ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.
-module(pervon).
-behaviour(gen_fsm).

-include_lib("inets/src/http_lib/http_internal.hrl").

% Interface
-export([start_link/1]).
-export([buf/3, stop/1]).
-export([content/3, session/4]).
-export([filename/4]).
% States
-export([perving/2, perving/3, archiving/2]).
% Behaviours
-export([init/1, handle_event/3, handle_sync_event/4,
        handle_info/3, terminate/3, code_change/4]).

-record(state, {
        saddr,
        sport,
        daddr,
        dport,
        f = fun(_) -> ok end,
        debug = true,
        data
    }).

-define(FILEPATH, "priv/files").
-define(TRACEPATH, "priv/tmp/").

-define(MAXHDRLEN, 8 * 1024).       % 8K, mimic Apache
-define(MAXCHUNKLEN, nolimit).      % XXX
-define(TIMEOUT, 5 * 1000 * 60).    % 5 minutes, HTTP pipeline timeout


%%--------------------------------------------------------------------
%%% Interface
%%--------------------------------------------------------------------
buf(Pid, SeqNo, Data) when is_binary(Data) ->
    gen_fsm:sync_send_event(Pid, {data, {SeqNo, Data}}).

stop(Pid) ->
    gen_fsm:sync_send_event(Pid, stop).


%%--------------------------------------------------------------------
%%% Callbacks
%%--------------------------------------------------------------------
start_link({{Saddr, Sport}, {Daddr, Dport}}) ->
    {ok, Pid} = gen_fsm:start(?MODULE, [{{Saddr, Sport}, {Daddr, Dport}}], []),
    erlang:monitor(process, Pid),
    {ok, Pid}.

init([{{Saddr, Sport}, {Daddr, Dport}}]) ->
    process_flag(trap_exit, true),
    error_logger:info_report([
        {session, start},
        {connection, session(Saddr, Sport, Daddr, Dport)}
    ]),
    {ok, perving, #state{
            saddr = Saddr,
            sport = Sport,
            daddr = Daddr,
            dport = Dport,
            data = gb_trees:empty()
        }}.


handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {next_state, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.


%%--------------------------------------------------------------------
%%% States
%%--------------------------------------------------------------------
perving({data, {SeqNo, Data}}, _From, #state{data = Payload} = State) ->
    case gb_trees:is_defined(SeqNo, Payload) of
        true ->
            {reply, ok, perving, State#state{
                data = Payload
            }, ?TIMEOUT};
        false ->
            {reply, ok, perving, State#state{
                data = gb_trees:enter(SeqNo, Data, Payload)
            }, ?TIMEOUT}
    end;
perving(stop, _From, State) ->
    {reply, ok, archiving, State, 0}.

perving(timeout, State) ->
    error_logger:info_report([{timeout, ?TIMEOUT}]),
    {next_state, archiving, State, 0}.

archiving(timeout, #state{
        saddr = Saddr,
        sport = Sport,
        daddr = Daddr,
        dport = Dport
    } = State) ->
    try dump(State) of
        _ -> ok
    catch
        throw:Error -> Error;
        Type:_Error -> error_logger:error_report([
                    {session, Type},
                    {connection, session(Saddr, Sport, Daddr, Dport)}
                ])
    end,

    {stop, shutdown, State}.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
filename(Path, Type, Name, Subtype) ->
    {A,B,C} = erlang:now(),
    filename:join([Path, Type, Name]) ++ "-"  ++
    integer_to_list(A) ++ integer_to_list(B) ++ integer_to_list(C) ++
    "." ++ Subtype.

content_type(undefined) ->
    {"text", "txt"};
content_type(String) ->
    type(hd(string:tokens(String, ";"))).

% Source: http://www.iana.org/assignments/media-types/
%   application
%   audio
%   example
%   image
%   message
%   model
%   multipart
%   text
%   video
type("application/" ++ Type) ->
    subtype("application", Type);
type("audio/" ++ Type) ->
    subtype("audio", Type);
type("image/" ++ Type) ->
    subtype("image",Type);
type("text/" ++ Type) ->
    subtype("text",Type);
type("video/" ++ Type) ->
    subtype("video",Type);
type(Type) ->
    subtype([],Type).

subtype(Type, "jpeg") -> {Type,"jpg"};
subtype(Type, "mpeg") -> {Type,"mpg"};
subtype(Type, "plain") -> {Type,"txt"};
subtype(Type, "quicktime") -> {Type,"mov"};
subtype(Type, "x-flv") -> {Type,"flv"};

subtype(Type, Subtype) ->
    case re:run(Subtype, "^[a-zA-Z0-9.+-]{1,128}+$", [{capture, none}]) of
        match -> {Type,Subtype};
        nomatch -> throw([{unsupported, {Type,Subtype}}])
    end.

dump(#state{
        debug = Debug,
        saddr = Saddr,
        sport = Sport,
        daddr = Daddr,
        dport = Dport,
        data = Data
    }) ->
    Body = list_to_binary(gb_trees:values(Data)),
    Name = session(Saddr, Sport, Daddr, Dport),

    error_logger:info_report([
        {session, finished},
        {connection, session(Saddr, Sport, Daddr, Dport)},
        {payload_size, byte_size(Body)}
    ]),

    case Debug of
        true ->
            ok = file:write_file(filename(?TRACEPATH, [], Name, "http"), Body);
        false ->
            ok
    end,

    content(?FILEPATH, Name, Body).

content(_Path, _Name, <<>>) ->
    ok;
content(Path, Name, Payload) ->
    {ok, {_Version, _Code, _Response, Headers, Body}} = httpc_response:parse([Payload, ?MAXHDRLEN, true]),

    {Hdr, Len, Content, Rest} = case {Headers#http_response_h.'transfer-encoding',
            Headers#http_response_h.'content-length'} of
        {"chunked", "-1"} ->
            {ok, {ChunkHdr, Chunk}} = http_chunk:decode(Body, ?MAXCHUNKLEN, ?MAXHDRLEN),
            Headers1 = http_chunk:handle_headers(Headers, ChunkHdr),
            N = list_to_integer(Headers1#http_response_h.'content-length'),
            <<B1:N/bytes, B2/binary>> = Chunk,
            {Headers1, N, B1, B2};
        {_, "-1"} ->
            {Headers, byte_size(Body), Body, <<>>};
        {_, Size} ->
            N = list_to_integer(Size),
            <<B1:N/bytes, B2/binary>> = Body,
            {Headers, N, B1, B2}
    end,

    case Len of
        0 ->
            ok;
        M when M > byte_size(Content) ->
            throw([{file, truncated}, {length, Len}, {byte_size, byte_size(Content)}]);
        _ ->
            {Type, Subtype} = content_type(Hdr#http_response_h.'content-type'),
            Filename = filename(Path, Type, Name, Subtype),
            error_logger:info_report([{file, Filename}, {'content-length', Len}, {bytes, byte_size(Content)}]),
            ok = write_content(Filename, Content)
    end,

    content(Path, Name, Rest).

write_content(Filename, Content) ->
    ok = filelib:ensure_dir(Filename),
    ok = file:write_file(Filename, Content).


% Established socket ntoa
session(Saddr, Sport, Daddr, Dport) ->
    inet_parse:ntoa(Saddr) ++ ":" ++ integer_to_list(Sport) ++ "-" ++
    inet_parse:ntoa(Daddr) ++ ":" ++ integer_to_list(Dport).

