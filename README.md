# Adventures in learning Erlang

## Running

    cd src
    # compile all modules
    make
    # or just run a program from the Makefile
    make echo
    
## Running the chat_server and clients

Open three terminal windows:

Terminal 1:

`make chat_server`

Terminal 2:

    erl -sname client1 -setcookie pass
    % the erlang console opened
    chat_client:start("Jacek", {chat_server, serv@shiny}).

or, easier:

`make chat_client1`

Terminal 3:

`make chat_client2`
