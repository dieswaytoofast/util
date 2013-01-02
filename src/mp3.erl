%%%-------------------------------------------------------------------
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @copyright (C) 2012 Mahesh Paolini-Subramanya
%%% @doc Module serving mp3 functions
%%% @end
%%%-------------------------------------------------------------------
-module(mp3).
-author('Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>').

-define(V1, 3).
-define(V2, 2).
-define(V25, 0).

-define(L1, 3).
-define(L2, 2).
-define(L3, 1).

-include("defaults.hrl").

%% Validations
-export([validate/1]).

%%% @doc  Check if the mp3 is valid and gives a reason if it's not
-spec validate(audio_path()) -> ok | error().
validate(Path) ->
    case file:open(Path, [read, raw, binary]) of
        {ok, File} ->
            Result = case file:read(File, 10) of
                %% ID3 Metadata
                %% ID3v2/file identifier   "ID3" 
                %% ID3v2 version           $03 00
                %% ID3v2 flags             %abc00000
                %% ID3v2 size              4 * %0xxxxxxx
                {ok, <<"ID3", 
                       _MajorVersion:1/binary, 
                       _Rev:1/binary, 
                       _Flag:1/binary, 
                       2#0:1, S1:7,
                       2#0:1, S2:7,
                       2#0:1, S3:7,
                       2#0:1, S4:7>>} ->
                    Size = ((S1 bsl 21) bor (S2 bsl 14) bor (S3 bsl 7) bor S4),
                    case file:read(File, Size) of
                        {ok, <<_:Size/binary>>} ->
                            case check_header(File) of
                                ok ->
                                    ok;
                                HeaderError ->
                                    HeaderError
                            end;
                        Error ->
                            {error, {invalid_id3, Error}}
                    end;
                {ok, <<"ID3", _Data/binary>>} ->
                    {error, invalid_id3};
                %% Header  (without ID metadata)
                {ok, _Header} ->
                    check_header(File);
                Error ->
                    {error, {file_too_short, Error}}
            end,
            file:close(File),
            Result;
        Error ->
            Error
    end.

%%% @see  http://www.mpgedit.org/mpgedit/mpeg_format/mpeghdr.htm
-spec check_header(file:io_device()) -> ok | error().
check_header(File) ->
    case file:read(File, 3) of
        {ok, FirstThree} ->
            check_header(File, FirstThree);
        eof ->
            {error, header_not_found};
        {error, _} ->
            {error, header_not_found}
    end.

%%% @see  http://www.mpgedit.org/mpgedit/mpeg_format/mpeghdr.htm
-spec check_header(file:io_device(), binary()) -> ok | error().
check_header(File, FirstThree) ->
    case file:read(File, 1) of
        {ok, Four} ->
            FourBytes = <<FirstThree/binary, Four/binary>>,
            case FourBytes of
                <<2#11111111111:11, _:21>> ->
                    case file:position(File, cur) of
                        {ok, CurrentPosition} ->
                            try 
                                case confirm_header(FourBytes, File) of
                                    ok ->
                                        ok;
                                    {error, _} ->
                                        case file:position(File, CurrentPosition) of
                                            {ok, CurrentPosition} ->
                                                <<_:1/binary, NextThree/binary>> = FourBytes,
                                                check_header(File, NextThree);
                                            {error, _} ->
                                                {error, header_not_found}
                                        end
                                end
                            catch
                                _:Error ->
                                    {error, {Error, erlang:get_stacktrace()}}
                            end;
                        {error, _} ->
                            {error, header_not_found}
                    end;
                <<_:1/binary, NextThree/binary>> ->
                    check_header(File, NextThree)
            end;
        eof ->
            {error, header_not_found};
        {error, _} ->
            {error, header_not_found}
    end.

-spec confirm_header(binary(), file:io_device()) -> ok | error().
confirm_header(<<2#11111111111:11, 2#01:2, 
                 _Tail:19>>, 
               _File) ->
    {error, <<"Reserved Audio Version">>};
confirm_header(<<2#11111111111:11,
                 _AudioVersion:2,
                 2#00:2, 
                 _Tail:17>>, 
               _File) ->
    {error, <<"Reserved Layer Descriptor">>};
confirm_header(<<2#11111111111:11, 
                 _AudioVersion:2,
                 _Layer:2,
                 _Protection:1,
                 2#1111:4, 
                 _Tail:12>>, 
               _File) ->
    {error, <<"Invalid BitRate">>};
confirm_header(<<2#11111111111:11, 
                 _AudioVersion:2,
                 _Layer:2,
                 _Protection:1,
                 2#0000:4, 
                 _Tail:12>>, 
               _File) ->
    {error, <<"Unsupported BitRate">>};
confirm_header(<<2#11111111111:11, 
                 _AudioVersion:2,
                 _Layer:2,
                 _Protection:1,
                 _BitRate:4,
                 2#11:2, 
                 _Tail:10>>, 
               _File) ->
    {error, <<"Invalid SampleRate">>};
confirm_header(<<2#11111111111:11, 
                 Version:2,
                 Layer:2,
                 _Protection:1,
                 BitRateCode:4,
                 SampleRateCode:2,
                 PaddingCode:1,
                 _Tail:9>>, 
               File) ->
    BitRate = bitrate(BitRateCode, Version, Layer),
    SampleRate = sample_rate(SampleRateCode, Version),
    Padding = padding(PaddingCode, Layer),
    FrameLengthInBytes = erlang:trunc(
            case Layer of
                ?L1 ->
                    (12000 * BitRate / SampleRate + Padding) * 4;
                _OtherLayer ->
                    144000 * BitRate / SampleRate + Padding
            end),
    %%NOTE: Header is included in total length
    case file:read(File, FrameLengthInBytes - 4) of 
        {ok, _Frame} ->
            case file:read(File, 2) of
                {ok, <<2#11111111111:11, _:5>>} ->
                    ok;
                {ok, <<_X:8, _Y:8>>} ->
                    {error, <<"Invalid header in the next frame">>};
                eof ->
                    {error, <<"File too short">>};
                {error, _} ->
                    {error, <<"File too short">>}
            end;
        eof ->
            {error, <<"File too short">>};
        {error, _} ->
            {error, <<"File too short">>}
    end.

padding(0, _) -> 0;
padding(_, ?L1) -> 32;
padding(_, _) -> 8.

sample_rate(0, ?V1) -> 44100;
sample_rate(1, ?V1) -> 48000;
sample_rate(2, ?V1) -> 32000;
sample_rate(0, ?V2) -> 22050;
sample_rate(1, ?V2) -> 24000;
sample_rate(2, ?V2) -> 16000;
sample_rate(0, ?V25) -> 11025;
sample_rate(1, ?V25) -> 12000;
sample_rate(2, ?V25) -> 8000.

bitrate(1, ?V1, ?L1) ->     32;
bitrate(2, ?V1, ?L1) ->     64;
bitrate(3, ?V1, ?L1) ->     96;
bitrate(4, ?V1, ?L1) ->     128;
bitrate(5, ?V1, ?L1) ->     160;
bitrate(6, ?V1, ?L1) ->     192;
bitrate(7, ?V1, ?L1) ->     224;
bitrate(8, ?V1, ?L1) ->     256;
bitrate(9, ?V1, ?L1) ->     288;
bitrate(10, ?V1, ?L1) ->     320;
bitrate(11, ?V1, ?L1) ->     352;
bitrate(12, ?V1, ?L1) ->     384;
bitrate(13, ?V1, ?L1) ->     416;
bitrate(14, ?V1, ?L1) ->     448;
bitrate(1, ?V1, ?L2) ->     32;
bitrate(2, ?V1, ?L2) ->     48;
bitrate(3, ?V1, ?L2) ->     56;
bitrate(4, ?V1, ?L2) ->     64;
bitrate(5, ?V1, ?L2) ->     80;
bitrate(6, ?V1, ?L2) ->     96;
bitrate(7, ?V1, ?L2) ->     112;
bitrate(8, ?V1, ?L2) ->     128;
bitrate(9, ?V1, ?L2) ->     160;
bitrate(10, ?V1, ?L2) ->     192;
bitrate(11, ?V1, ?L2) ->     224;
bitrate(12, ?V1, ?L2) ->     256;
bitrate(13, ?V1, ?L2) ->     320;
bitrate(14, ?V1, ?L2) ->     384;
bitrate(1, ?V1, ?L3) ->     32;
bitrate(2, ?V1, ?L3) ->     40;
bitrate(3, ?V1, ?L3) ->     48;
bitrate(4, ?V1, ?L3) ->     56;
bitrate(5, ?V1, ?L3) ->     64;
bitrate(6, ?V1, ?L3) ->     80;
bitrate(7, ?V1, ?L3) ->     96;
bitrate(8, ?V1, ?L3) ->     112;
bitrate(9, ?V1, ?L3) ->     128;
bitrate(10, ?V1, ?L3) ->     160;
bitrate(11, ?V1, ?L3) ->     192;
bitrate(12, ?V1, ?L3) ->     224;
bitrate(13, ?V1, ?L3) ->     256;
bitrate(14, ?V1, ?L3) ->     320;
bitrate(1, _, ?L1) ->     32;
bitrate(2, _, ?L1) ->     48;
bitrate(3, _, ?L1) ->     56;
bitrate(4, _, ?L1) ->     64;
bitrate(5, _, ?L1) ->     80;
bitrate(6, _, ?L1) ->     96;
bitrate(7, _, ?L1) ->     112;
bitrate(8, _, ?L1) ->     128;
bitrate(9, _, ?L1) ->     144;
bitrate(10, _, ?L1) ->     160;
bitrate(11, _, ?L1) ->     176;
bitrate(12, _, ?L1) ->     192;
bitrate(13, _, ?L1) ->     224;
bitrate(14, _, ?L1) ->     256;
bitrate(1, _, _) ->     8;
bitrate(2, _, _) ->     16;
bitrate(3, _, _) ->     24;
bitrate(4, _, _) ->     32;
bitrate(5, _, _) ->     40;
bitrate(6, _, _) ->     48;
bitrate(7, _, _) ->     56;
bitrate(8, _, _) ->     64;
bitrate(9, _, _) ->     80;
bitrate(10, _, _) ->     96;
bitrate(11, _, _) ->     112;
bitrate(12, _, _) ->     128;
bitrate(13, _, _) ->     144;
bitrate(14, _, _) ->     160.
