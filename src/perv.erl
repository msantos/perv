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
-module(perv).
-behaviour(gen_server).

-include_lib("inets/src/http_lib/http_internal.hrl").
-include("pkt.hrl").

-define(SERVER, ?MODULE).

-export([start/0, start/1, stop/0]).
-export([dev/0, getip/1]).
-export([start_link/0, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).

-record(state, {
        c   % monitored connections
    }).

-define(EPCAP_DEFAULT_ARG, [{filter, "tcp and src port 80"},
                {promiscuous, true},
                {chroot, "priv/tmp"}]).


start() ->
    start_link().
start(Options) ->
    start_link(Options).

stop() ->
    gen_server:call(?SERVER, stop).

start_link() ->
    start_link([]).

start_link(Options) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Options], []).

init([Options]) ->
    {Dev, Address} = dev(),

    EpcapArgs = case Options of
        exclude ->
            [{filter, "tcp and src port 80 and not host " ++ inet_parse:ntoa(Address)}];
        Opt when is_list(Opt) ->
            Options
    end,

    epcap:start(EpcapArgs ++ [{interface, Dev}] ++ ?EPCAP_DEFAULT_ARG),
    {ok, #state{c = dict:new()}}.


handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

% Sniffed packet
handle_info({packet, _DLT, _Time, Length, Packet}, #state{c = C} = State)
    when byte_size(Packet) == Length ->
    P = pkt:decapsulate(Packet),
    C1 = match(P, C),
    {noreply, State#state{c = C1}};

% Connection closed
handle_info({'DOWN', _Ref, process, Pid, _Reason}, #state{
        c = Connections
    } = State) ->
    {noreply, State#state{
            c = dict:filter(
                fun (_,V) when V == Pid -> false;
                    (_,_) -> true
                end,
                Connections)
        }};

% WTF?
handle_info(Info, State) ->
    error_logger:error_report([{wtf, Info}]),
    {noreply, State}.

terminate(_Reason, #state{c = C}) ->
    epcap:stop(),
    dict:map(fun(_K,V) -> pervon:stop(V) end, C),
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
% closed connections
match([ #ether{},
        #ipv4{
            saddr = Saddr,
            daddr = Daddr,
            hl = HL,
            len = Len
        },
        #tcp{
            sport = 80,
            dport = Dport,
            seqno = SeqNo,
            off = Off,
            ack = ACK,
            rst = RST,
            fin = FIN
        },
        Payload], Connections) when ( RST =:= 1 andalso ACK =:= 0 )
            orelse ( FIN =:= 1 andalso ACK =:= 1 ) ->

    Key = {{Saddr, 80}, {Daddr, Dport}},
    case dict:find(Key, Connections) of
        {ok, Pid} ->
            buf(Pid, SeqNo, Payload, Len - (HL * 4) - (Off * 4)),
            pervon:stop(Pid),
            dict:erase(Key, Connections);
        error ->
            Connections
    end;

% connections in ESTABLISHED state
match([ #ether{},
        #ipv4{
            saddr = Saddr,
            daddr = Daddr,
            hl = HL,
            len = Len
        },
        #tcp{
            sport = 80,
            dport = Dport,
            seqno = SeqNo,
            off = Off,
            syn = 0,
            rst = 0,
            fin = 0,
            ack = 1
        },
        Payload], Connections) ->

    Key = {{Saddr, 80}, {Daddr, Dport}},
    {P, C} =  case dict:find(Key, Connections) of
        {ok, Pid} ->
            {Pid, Connections};
        error ->
            {ok, Pid} = pervon:start_link(Key),
            {Pid, dict:store(Key, Pid, Connections)}
    end,
    buf(P, SeqNo, Payload, Len - (HL * 4) - (Off * 4)),
    C;
match(_, Connections) ->
    Connections.


% Send the packet payload to the gen_fsm.
% 
% As an optimization, do not send packets with 0 byte payloads.
%
% Some packets have a payload that does not match the size in the
% header. For example, libpcap will sometimes return 6 extra trailing
% bytes.
%
% In the case of payload exceeding the expected packet size,
% truncate the payload.
%
% If the expected packet size is larger than the payload, crash. 
% 
buf(_Pid, _SeqNo, _Payload, PayloadSize) when PayloadSize =:= 0 ->
    ok;
buf(Pid, SeqNo, Payload, PayloadSize) when PayloadSize =:= byte_size(Payload) ->
    pervon:buf(Pid, SeqNo, Payload);
buf(Pid, SeqNo, Payload, PayloadSize) when PayloadSize < byte_size(Payload) ->
    error_logger:info_report([{truncating, SeqNo}, {from, byte_size(Payload)}, {to, PayloadSize}]),
    pervon:buf(Pid, SeqNo, <<Payload:PayloadSize/bytes>>).


% Try to autostupidly figure out the local host's network device.
% Guaranteed to annoy.
dev() ->
    {ok, Devs} = inet:getiflist(),
    Ifs = [ {N, getip(N)} || N <- Devs ],
    hd(lists:filter(
        fun ({_, {127,_,_,_}}) -> false;
            ({_, {169,_,_,_}}) -> false;
            ({_, {_,_,_,_}}) -> true
        end, Ifs)).

getip(Dev) ->
    {ok, Addr} = inet:ifget(Dev, [addr]),
    proplists:get_value(addr, Addr).

