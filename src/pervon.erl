%% Copyright (c) 2010-2012, Michael Santos <michael.santos@gmail.com>
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
-export([perving/2]).
% Behaviours
-export([init/1, handle_event/3, handle_sync_event/4,
        handle_info/3, terminate/3, code_change/4]).


-define(FILEPATH, "priv/files").
-define(TRACEPATH, "priv/tmp/").

-define(MAXHDRLEN, 8 * 1024).       % 8K, mimic Apache
-define(MAXCHUNKLEN, nolimit).      % XXX
-define(TIMEOUT, 5 * 1000 * 60).    % 5 minutes, HTTP pipeline timeout
-define(TIMEWAIT, 10 * 1000).       % 10 seconds, allow for delayed
                                    %  packets when shutting down

-record(state, {
        saddr,
        sport,
        daddr,
        dport,
        timeout = ?TIMEOUT,
        debug = true,
        data
    }).


%%--------------------------------------------------------------------
%%% Interface
%%--------------------------------------------------------------------
buf(Pid, SeqNo, Data) when is_binary(Data) ->
    gen_fsm:send_event(Pid, {data, {SeqNo, Data}}).

stop(Pid) ->
    gen_fsm:send_event(Pid, stop).


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

terminate(_Reason, _StateName, #state{
        saddr = Saddr,
        sport = Sport,
        daddr = Daddr,
        dport = Dport
    } = State) ->
    try dump(State) of
        _ -> ok
    catch
        Type:_Error -> error_logger:error_report([
                    {session, Type},
                    {connection, session(Saddr, Sport, Daddr, Dport)}
                ]),
            ok
    end.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.


%%--------------------------------------------------------------------
%%% States
%%--------------------------------------------------------------------

% There seems to be a race condition in receiving data/stopping
% the fsm. Occasionally, data or stop events will be sent after
% stop has been called.
%
% 1. Since events are delivered async, they may be received
%    out of order. The events could be sent synchronously,
%    but:
%
% 2. The closing FIN packet may be delivered out of order
%    (in which case the OS will not ACK it) or it may be
%    a duplicate. 
%
% The TIME_WAIT interval should allow enough time for
% additional events to be processed.
%
perving({data, {SeqNo, Data}}, #state{
        data = Payload,
        timeout = Timeout
    } = State) ->
    case gb_trees:is_defined(SeqNo, Payload) of
        true ->
            {next_state, perving, State#state{
                data = Payload
            }, Timeout};
        false ->
            {next_state, perving, State#state{
                data = gb_trees:enter(SeqNo, Data, Payload)
            }, Timeout}
    end;
perving(stop, State) ->
    {next_state, perving, State#state{
            timeout = ?TIMEWAIT
        }, ?TIMEWAIT};
perving(timeout, State) ->
    {stop, normal, State}.


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
        nomatch -> {Type,"warning-unsupported-subtype"}
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
    {ok, {_Version, Code, _Response, Headers, Body}} = httpc_response:parse([Payload, ?MAXHDRLEN, true]),

    {Hdr, Content, Rest} = case Headers#http_response_h.'content-length' of

        "-1" when Headers#http_response_h.'transfer-encoding' == "chunked" ->
            {ok, {ChunkHdr, Chunk}} = http_chunk:decode(Body, ?MAXCHUNKLEN, ?MAXHDRLEN),
            Headers1 = http_chunk:handle_headers(Headers, ChunkHdr),
            N = list_to_integer(Headers1#http_response_h.'content-length'),
            <<B1:N/bytes, B2/binary>> = Chunk,
            {Headers1, B1, B2};

        % Some servers do not send a Content-Length header when replying with a
        % "304 Not Modified" response in a persistent connection. 304 responses
        % cannot have a body (see RFC 2616).
        "-1" when Code =:= 304 ->
            {Headers, <<>>, Body};

        % No Content-Length header; the end of response is indicated by closing
        % the socket
        "-1" ->
            {Headers, Body, <<>>};

        Size ->
            N = list_to_integer(Size),
            <<B1:N/bytes, B2/binary>> = Body,
            {Headers, B1, B2}
    end,

    {Type, Subtype} = content_type(Hdr#http_response_h.'content-type'),
    Filename = filename(Path, Type, Name, Subtype),
    ok = write_content(Filename, Content),

    content(Path, Name, Rest).

write_content(_Filename, Content) when byte_size(Content) =:= 0 ->
    ok;
write_content(Filename, Content) ->
    error_logger:info_report([{file, Filename}, {size, byte_size(Content)}]),
    ok = filelib:ensure_dir(Filename),
    ok = file:write_file(Filename, Content).


% Established socket ntoa
session(Saddr, Sport, Daddr, Dport) ->
    inet_parse:ntoa(Saddr) ++ ":" ++ integer_to_list(Sport) ++ "-" ++
    inet_parse:ntoa(Daddr) ++ ":" ++ integer_to_list(Dport).

