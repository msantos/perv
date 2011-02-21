%% Copyright (c) 2010-2011, Michael Santos <michael.santos@gmail.com>
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
-module(peep).

-define(IMGPATH, "priv/files/image").

-export([start/0, stop/0]).
% web interface
-export([show/3]).

start() ->
    case file:make_symlink("../files/image", "priv/www/img") of
        ok -> ok;
        {error,eexist} -> ok
    end,

    application:start(inets),
    inets:start(httpd, [
            {modules, [
                    mod_alias,
                    mod_esi,
                    mod_get,
                    mod_log,
                    mod_disk_log
                ]},
            {port, 8889},
            {server_name, "localhost"},
            {server_root, "priv/log"},
            {document_root, "priv/www"},
            {directory_index, ["index.html"]},
            {error_log, "error.log"},
            {security_log, "security.log"},
            {transfer_log, "transfer.log"},
            {erl_script_alias, {"/web", [peep]}}
        ]).

stop() ->
    inets:stop().


%%%
%%% web services
%%%
show(SessionID, _Env, _Input) ->
    L = filelib:fold_files(?IMGPATH, "(.png|.jpg)$", false, fun(X,Y) ->
                [{filelib:last_modified(X), X}|Y] end, []),
    SL = lists:sublist(lists:reverse(lists:keysort(1, L)), 20),
    Obj = [ json(N) || N <- SL ],

    mod_esi:deliver(SessionID, header(json)),
    JSON = lists:flatten(io_lib:format("{\"images\": [~s]}", [string:join(Obj, ",")])),
    mod_esi:deliver(SessionID, JSON).


%%%
%%% helper functions
%%%
header(json) ->
    "Content-Type: application/json\r\n\r\n".

iso_8601_fmt(DateTime) ->
    {{Year,Month,Day},{Hour,Min,Sec}} = DateTime,
    lists:flatten(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B",
            [Year, Month, Day, Hour, Min, Sec])).

json({TS, File}) ->
    Name = filename:basename(File),
    Date = iso_8601_fmt(TS),
    [SrcIP, DstIP, _] = string:tokens(Name, "-"),
    URI = "/img/" ++ Name,

    io_lib:format("{\"date\":\"~s\",\"src\":\"~s\",\"dst\":\"~s\",\"uri\":\"~s\"}",
        [Date, SrcIP, DstIP, URI]).

