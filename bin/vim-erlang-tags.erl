#!/usr/bin/env escript
-mode(compile).
main(_Args) ->
    Help =
"The vim-erlang-tags.erl script has been moved to vim_erlang_tags.erl.
Please use that script instead."
, io:format("~s", [Help]).
