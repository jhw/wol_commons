-module(wol_datetime).

-export([now_utc/0,
	 add_seconds/2,
	 timedelta/2]).

now_utc() ->
    calendar:now_to_datetime(erlang:timestamp()).

add_seconds(DateTime, Seconds) ->
    calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(DateTime)+Seconds).

timedelta(DT0, DT1) ->
    calendar:datetime_to_gregorian_seconds(DT1)-calendar:datetime_to_gregorian_seconds(DT0).
