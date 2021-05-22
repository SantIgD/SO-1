-module(emptyMB).
-export([empty_mailbox/0]).


empty_mailbox() ->
    
    receive
        _ -> io:format("vaciando~n"), empty_mailbox()

    after
      0 -> ok
    end.